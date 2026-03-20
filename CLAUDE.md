# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**dubsar** is a compiler frontend (WiP) for a custom programming language also called "dubsar". It implements lexing, parsing, AST construction, pretty-printing via a visitor pattern, and a semantic pass with OCaml-inspired type inference using Hindley-Milner unification and row types. The language supports functions, variables, structs with inheritance, interfaces, methods, control flow (if/else, for, for-range, continue, break), tuples, and type inference.

## Build System

This project uses **Meson** with **Clang** (required — the build will error if another compiler is detected). Flex and **Bison ≥ 3.0** are also required. C++20 is used. On macOS the system Bison (2.x) is too old; install a newer version via Homebrew (`brew install bison`). Meson will detect it automatically via the Homebrew opt path. CLI argument parsing uses [argparse](https://github.com/p-ranav/argparse) (v3.2, header-only, fetched automatically via Meson WrapDB).

```bash
# Configure (first time or after meson.build changes)
meson setup build

# Build
ninja -C build

# Run on a .dub source file
./build/src/dubsar examples/example.dub

# Run without type checking (parse + print only)
./build/src/dubsar --no-check examples/example.dub

# Run tests
ninja -C build test
```

The build system defines roundtrip tests (parse → print → parse → compare) and error tests (verify non-zero exit). Test runner: `tests/run_test.py`.

## Architecture

The pipeline is: `.dub` source file → **Lexer** → **Parser** → **AST** → **Resolver** → **Type Checker** → **Printer** → stdout.

The `--no-check` flag skips the resolver and type checker passes.

### Source files (`src/`)

- `lexer.l` — Flex lexer. Keywords: `fun`, `var`, `ref`, `return`, `for`, `type`, `struct`, `string`, `int`, `bool`, `byte`, `float`, `double`, `char`, `integer`, `interface`, `if`, `else`, `continue`, `break`. Operators include arithmetic (including `%`), comparison, logical, pre/post increment/decrement, compound assignment (`+=`, `-=`, `*=`, `/=`), `::`, `->`, and indexing `[]`. Supports `//` and `/* */` comments. Includes the generated `parser.hpp` for `YYSTYPE` and token constants (no local union definition).
- `parser.y` — Bison grammar. Produces a `program_node*` in the global `root` variable. Handles: function/method/type declarations at top level, `var` declarations, `for`/for-range loops, `if`/`else`, `return`, `continue`, `break`, compound statements, tuple declarations/assignments, and expressions. Uses `%code requires {}` to embed forward declarations (from `parser_types.h`) into the generated `parser.hpp`. Uses `%destructor` rules (one per union type tag) to delete raw pointers discarded during error recovery. The `loc()` template sets `node->line = yylineno` on every allocated AST node.
- `ast.h` / `ast.cpp` — AST node class hierarchy. `program_node` is the root. All nodes have `int line` for source locations. All nodes implement `accept(visitor&)` for the visitor pattern. `ast.cpp` defines the global `root` variable.
- `visitor.h` — Abstract `visitor` base class with `visit()` overloads for every node type.
- `printer.h` / `printer.cpp` — `printer` concrete visitor for AST pretty-printing. Adds parentheses around all binary ops for round-trip stability (avoids any precedence ambiguity on re-parse).
- `parser_types.h` — Forward declarations of all AST node classes. Included via `%code requires {}` in `parser.y` so it appears in the generated `parser.hpp`, making the declarations available to any file that includes that header (including the lexer).
- `types.h` / `types.cpp` — Type IR hierarchy (`type_t` base, `prim_type_t`, `sized_int_type_t`, `type_var_t`, `fun_type_t`, `tuple_type_t`, `generic_type_t`, `row_type_t`, `named_type_t`). Includes `parse_type_string()` to convert opaque AST type strings into the type IR. Primitive types are singletons.
- `unify.h` / `unify.cpp` — Unification engine (`type_env`). Implements HM-style unification with occurs check, Rémy-style row unification for struct/interface polymorphism, and generalize/instantiate for let-polymorphism. Types use `shared_ptr` with union-find.
- `diagnostics.h` / `diagnostics.cpp` — Error/warning collection with source locations. Resolution errors are hard errors (exit 1); type inference mismatches are warnings (non-fatal).
- `symbol_table.h` / `symbol_table.cpp` — Scoped symbol table (`push_scope`/`pop_scope`/`bind`/`lookup`) and type registry (`register_type`/`lookup_type`) with function registry.
- `resolver.h` / `resolver.cpp` — Name resolution visitor. Two sub-passes: (1) register all type names, (2) fill in struct fields, interface methods, function signatures, resolve inheritance. Structs become closed rows, interfaces become open rows.
- `type_checker.h` / `type_checker.cpp` — Type inference visitor. Infers expression types via HM unification, binds variables in scoped symbol table, checks function/method bodies. Expression types are stored in a side table (`unordered_map<const ast_node*, type_ptr>`) to avoid modifying AST node classes. Method bodies have implicit access to their struct's fields. Generalization at top-level function boundaries.
- `main.cpp` — Entry point: uses [argparse](https://github.com/p-ranav/argparse) for CLI parsing (`--no-check`, positional input file), opens the file, calls `yyparse()`, runs resolver + type checker (unless `--no-check`), then calls `printer` on the AST root.

### Build-generated files (`build/src/`)

- `build/src/parser.cpp` and `build/src/parser.hpp` are generated by Bison (`-d` flag emits the header). `build/src/lexer.cpp` is generated by Flex. Do not edit these; edit `parser.y` and `lexer.l` instead.

### AST Node Hierarchy

```
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

```
type_t (abstract)
├── prim_type_t          — int, bool, byte, float, double, char, string (singletons)
├── sized_int_type_t     — integer<N> with signedness flag
├── type_var_t           — unification variable (id, bound, level)
├── fun_type_t           — (T1, T2, ...) -> T_ret
├── tuple_type_t         — (T1, T2, ...)
├── generic_type_t       — name + type args (e.g. vector<int>)
├── row_type_t           — { label1: T1, ... | tail } (tail = nullptr or type_var)
└── named_type_t         — reference to declared struct/interface
```

### Semantic Pass Design

- **Structs are closed rows, interfaces are open rows** — interface satisfaction works via row unification where the open tail binds to residual fields
- **`ref` is calling convention, not a type** — `ref int` has type `int`; `is_ref` stays on `param_node`/`symbol_entry` only
- **Expression types in side table** — avoids modifying 28+ AST node classes; printer and roundtrip tests unaffected
- **Method bodies bind struct fields** — methods have implicit access to their type's fields in scope
- **Top-level let-polymorphism** — un-annotated top-level functions get generalized (e.g. `fun id(x) { return x; }` → `∀α. α → α`)
- **Permissive inference** — undefined functions/variables get fresh type vars (no stdlib yet); type mismatches are warnings, not hard errors
- **Resolution errors are hard errors** — duplicate type/function names cause exit code 1

### Language Features

- **Functions**: inferred types (`fun f(x)`), explicit types (`fun f(x: int) -> int`)
- **Parameters**: by-value (`p`), by-value typed (`p: int`), by-ref (`p: ref`), by-ref typed (`p: int ref`)
- **Variables**: `var x = 10;`, `var x: int = 10;`, `var x: int;` (no init), tuple `var x, y = func();`, init-list `var v: vector<int> = {};`
- **Type declarations**: structs (`type point = struct { x: int; y: int; }`), inheritance (`struct : ParentType`), interfaces (`type reader = interface { read(sz: int) -> vector<byte>; }`)
- **Methods**: `fun TypeName::methodName(p: int) -> ReturnType { ... }`, called as `obj.method(args)`. Parameters are stored in `method_decl_node` (same as `func_decl_node`) and printed by the shared `print_params` helper.
- **Field access**: `obj.field`, supports chaining (`obj.child.value`) and assignment (`obj.field = expr`)
- **Qualified calls**: `ns::func(args)`
- **C-style for loop**: two forms (no parens, no semicolon before body): `for var i = 0; i < n; ++i { ... }` (var-decl init) and `for expr; expr; expr { ... }` (expression init, e.g. `for i = 1; i <= n; i = i + 1 { ... }`).
- **Range-based for**: `for var item = range(collection) { ... }`
- **If/else**: `if cond { ... }` / `if cond { ... } else { ... }`
- **Continue / Break**: `continue;`, `break;`
- **Base types**: `int`, `bool`, `byte`, `float`, `double`, `char`, `string` are reserved keywords usable in type positions
- **Sized integers**: `integer<64>` (signed), `integer<+64>` (unsigned); bare `integer` is not valid
- **Generic types**: `vector<int>`, `vector<byte>`, etc. are supported in type positions (parameters, return types, variable declarations)
- **Operators**: arithmetic (including `%`), comparison, logical (`&&`, `||`, `!`), compound assignment (`+=` `-=` `*=` `/=`), pre/post `++`/`--`, indexing `[]`
- **Tuples**: returned as `return a, b;`, destructured as `var x, y = f();`, assigned with general lvalue LHS (`v[i], v[j] = v[j], v[i];`)

### Test Infrastructure

```
tests/
  run_test.py              — Python test runner
  fixtures/
    valid/                 — 35 roundtrip test fixtures (parse→print→parse→compare)
    invalid/               — 9 error test fixtures (expect non-zero exit)
examples/
  example.dub              — Core language features (also a roundtrip test)
  example2.dub             — Tuples, vectors, for-range
  interface.dub            — Interface syntax
  sieve.dub                — Sieve of Eratosthenes
  collections.dub          — Structs, methods, generics, loops, tuples, compound assign
  quicksort.dub            — In-place quicksort (also a roundtrip test)
```
