# CLAUDE.md

Guidance for Claude Code (claude.ai/code) when working in this repository.

## Project Overview

**dubsar** is a work-in-progress compiler frontend for a language of the same
name. It implements lexing, parsing, AST construction, pretty-printing through a
visitor, and a semantic pass with OCaml-inspired type inference (Hindley-Milner
unification plus row types). The language has functions, variables, structs with
inheritance, interfaces, methods, control flow (`if`/`else`, `for`, for-range,
`continue`, `break`), tuples, and type inference.

## Build System

Meson with **Clang** (the build errors out on any other compiler), Flex, and
**Bison ≥ 3.0**. C++20. The macOS system Bison is 2.x — install a newer one with
`brew install bison`; Meson finds it through the Homebrew opt path. CLI parsing
uses [argparse](https://github.com/p-ranav/argparse) 3.2 (header-only, fetched
from Meson WrapDB).

```bash
meson setup build              # first time, or after editing meson.build
ninja -C build
./build/src/dubsar examples/example.dub
./build/src/dubsar --no-check examples/example.dub   # parse and print only
meson test -C build
```

Tests are roundtrip (parse → print → parse → compare) and error (non-zero exit).
Runner: `tests/run_test.py`.

## Architecture

Pipeline: `.dub` source → **Lexer** → **Parser** → **AST** → **Resolver** →
**Type Checker** → **Printer** → stdout. `--no-check` skips the resolver and type
checker.

### Source files (`src/`)

| File                          | Role                                                                                                                                                                                                                                                                 |
|-------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `lexer.l`                     | Flex lexer. Includes the generated `parser.hpp` for `YYSTYPE` and the token constants.                                                                                                                                                                               |
| `parser.y`                    | Bison grammar. Produces a `program_node` in the global `root`. `%code requires {}` embeds `ast_fwd.h` into `parser.hpp`. `%destructor` rules (one per union type tag) delete raw pointers that error recovery discards. `loc()` stamps `yylineno` on every new node. |
| `ast_fwd.h`                   | Forward declarations of every AST class. The single source of that list, included by `visitor.h` and by `parser.hpp`.                                                                                                                                                |
| `ast.h` / `ast.cpp`           | AST node hierarchy rooted at `ast_node`, which carries `int line`. Every node implements `accept(visitor&)`. `ast.cpp` defines the global `root`.                                                                                                                    |
| `visitor.h`                   | Abstract `visitor` with one `visit()` overload per node type.                                                                                                                                                                                                        |
| `printer.h` / `printer.cpp`   | `printer` visitor. Parenthesizes every binary op for roundtrip stability. `print_list()` emits any comma-separated node list.                                                                                                                                        |
| `types.h` / `types.cpp`       | Type IR (`type_t` base plus the subclasses below) and `parse_type_string()`, which turns an opaque AST type string into type IR. Primitives are singletons.                                                                                                          |
| `unify.h` / `unify.cpp`       | `type_env`: HM unification with occurs check, Remy-style row unification, and generalize/instantiate. Types are `shared_ptr` with union-find. Also holds `type_or_fresh()`.                                                                                          |
| `diagnostics.h` / `.cpp`      | Error and warning collection with source lines. `emit()` writes everything to a stream.                                                                                                                                                                              |
| `symbol_table.h` / `.cpp`     | Scoped symbol table plus the type and function registries. `type_info` owns a struct's own field row, its method row, and a parent pointer.                                                                                                                          |
| `resolver.h` / `resolver.cpp` | Name resolution over three passes (below).                                                                                                                                                                                                                           |
| `type_checker.h` / `.cpp`     | Type inference visitor. Expression types live in a side table (`unordered_map<const ast_node*, type_ptr>`), so AST classes stay untouched.                                                                                                                           |
| `main.cpp`                    | Opens the file, calls `yyparse()`, runs resolver plus type checker unless `--no-check`, emits diagnostics to stderr, prints the AST to stdout.                                                                                                                       |

Keywords: `fun`, `var`, `ref`, `return`, `for`, `type`, `struct`, `interface`,
`if`, `else`, `continue`, `break`, and the base types `string`, `int`, `bool`,
`byte`, `float`, `double`, `char`, `integer`.

`build/src/parser.cpp`, `build/src/parser.hpp`, and `build/src/lexer.cpp` are
generated. Edit `parser.y` and `lexer.l` instead.

### AST Node Hierarchy

```text
ast_node (int line)
├── expr_node
│   ├── identifier_node, number_node, string_node
│   ├── binary_op_node, unary_op_node, assign_node, compound_assign_node
│   ├── call_node, member_call_node, member_access_node, qualified_call_node
│   ├── index_node, tuple_expr_node, init_list_expr_node
│
├── stmt_node
│   ├── expr_stmt_node, var_decl_node, tuple_var_decl_node, tuple_assign_stmt_node
│   ├── return_stmt_node, compound_stmt_node
│   ├── for_stmt_node, for_range_stmt_node, continue_stmt_node, break_stmt_node
│   └── if_stmt_node
│
└── decl_node (extends stmt_node)
    ├── param_node, func_decl_node, method_decl_node
    ├── struct_field_node, type_decl_node, program_node
    └── type_body_node
        ├── struct_type_node
        └── interface_type_node (with interface_method_node)
```

### Type IR Hierarchy

```text
type_t (abstract)
├── prim_type_t          — int, bool, byte, float, double, char, string (singletons)
├── sized_int_type_t     — integer<N> with signedness flag
├── type_var_t           — unification variable (id, bound, level)
├── fun_type_t           — (T1, T2, ...) -> T_ret
├── tuple_type_t         — (T1, T2, ...)
├── generic_type_t       — name + type args (e.g. vector<int>)
├── row_type_t           — { label1: T1, ... | tail } (tail = nullptr or type_var)
└── named_type_t         — reference to a declared struct/interface
```

### Resolver Passes

| Pass             | Work                                                           |
|------------------|----------------------------------------------------------------|
| `register_names` | Registers every type name with empty field and method rows     |
| `fill_types`     | Fills struct fields and interface method signatures            |
| `link`           | Links struct parents, registers function and method signatures |

Because all types are complete before `link` runs, declaration order affects
nothing: a method may precede its type, and a struct may inherit from a type
declared later.

### Semantic Pass Design

- **Structs are closed rows, interfaces are open rows** — interface satisfaction
  works by row unification, where the open tail binds to the residual fields.
- **Inheritance is a parent pointer, not flattened fields** — `type_info` holds a
  struct's own fields; `find_field` walks the parent chain, so a child's field
  shadows the parent's. The resolver rejects inheritance cycles.
- **`ref` is calling convention, not a type** — `ref int` has type `int`; `is_ref`
  stays on `param_node` and `symbol_entry`.
- **Expression types in a side table** — avoids touching 28+ AST classes, so the
  printer and roundtrip tests are unaffected.
- **Method bodies bind struct fields** — including inherited ones.
- **Top-level let-polymorphism** — un-annotated top-level functions generalize,
  so `fun id(x) { return x; }` becomes `∀α. α → α`.
- **Permissive inference** — undefined functions and variables get fresh type
  vars (there is no stdlib yet).
- **Resolution errors are hard errors** (exit 1); type mismatches are warnings and
  still produce output. Both go to stderr.

Hard errors: duplicate type name, duplicate function name, duplicate field,
duplicate method on a type, undefined parent type, inheriting from an interface,
cyclic inheritance, method on an undefined type.

### Language Features

| Feature           | Forms                                                                                                                       |
|-------------------|-----------------------------------------------------------------------------------------------------------------------------|
| Functions         | `fun f(x)` inferred, `fun f(x: int) -> int` explicit                                                                        |
| Parameters        | `p`, `p: int`, `p: ref`, `p: int ref`, `p: ref int`                                                                         |
| Variables         | `var x = 10;`, `var x: int = 10;`, `var x: int;`, tuple `var x, y = f();`, init-list `var v: vector<int> = {};`             |
| Type declarations | `type point = struct { x: int; }`, `struct : ParentType`, `type reader = interface { read(sz: int) -> vector<byte>; }`      |
| Methods           | `fun TypeName::methodName(p: int) -> ReturnType { ... }`, called as `obj.method(args)`                                      |
| Field access      | `obj.field`, chained `obj.child.value`, assignable `obj.field = expr`                                                       |
| Qualified calls   | `ns::func(args)`                                                                                                            |
| C-style `for`     | `for var i = 0; i < n; ++i { ... }` and `for i = 1; i <= n; i = i + 1 { ... }` — no parens, no semicolon before the body    |
| Range-based `for` | `for var item = range(collection) { ... }`                                                                                  |
| `if` / `else`     | `if cond { ... }`, `if cond { ... } else { ... }`                                                                           |
| Base types        | `int`, `bool`, `byte`, `float`, `double`, `char`, `string` — reserved keywords, valid in type positions and as method names |
| Sized integers    | `integer<64>` signed, `integer<+64>` unsigned; bare `integer` is invalid                                                    |
| Generic types     | `vector<int>`, `vector<integer<64>>`, … in parameter, return, and variable positions                                        |
| Tuples            | `return a, b;`, `var x, y = f();`, general-lvalue assignment `v[i], v[j] = v[j], v[i];`                                     |

Operators: arithmetic including `%`, comparison, logical `&&` `||` `!`, compound
assignment `+=` `-=` `*=` `/=`, pre/post `++` and `--`, and indexing `[]`.

### Test Infrastructure

```text
tests/
  run_test.py              — Python test runner
  fixtures/
    valid/                 — 35 roundtrip fixtures (parse→print→parse→compare)
    invalid/               — 9 error fixtures (expect non-zero exit)
examples/
  example.dub              — core language features (also a roundtrip test)
  example2.dub             — tuples, vectors, for-range
  interface.dub            — interface syntax
  sieve.dub                — Sieve of Eratosthenes
  collections.dub          — structs, methods, generics, loops, tuples, compound assign
  quicksort.dub            — in-place quicksort (also a roundtrip test)
```

Several `valid/` fixtures exercise the parser with programs that do not type-check
(`logical_not.dub`, `sized_int.dub`, `tuple_typed_params.dub`, `base_types.dub`).
They emit warnings on stderr and still pass, because roundtrip tests compare
stdout.
