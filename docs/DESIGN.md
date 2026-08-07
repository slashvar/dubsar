# dubsar Design Document

Language goals, compiler architecture, type system rationale, and the reasoning
behind the main design decisions.

For field-level detail (AST node hierarchy, every keyword, operator precedence)
see [`CLAUDE.md`](../CLAUDE.md). For build instructions see
[`README.md`](../README.md).

---

## 1. Goals

**dubsar** is a work-in-progress compiler frontend for a language of the same
name. It covers lexing, parsing, AST construction, name resolution, type
inference, and pretty-printing. Code generation is not implemented.

Design influences:

- **OCaml / Hindley-Milner** — global type inference with let-polymorphism, so
  annotations stay optional.
- **Go-like syntax** — braces for blocks, no parentheses around `for`/`if`
  conditions, `var` for short declarations.
- **Structural typing via row types** — interfaces are satisfied structurally
  (as in Go), implemented by row-type unification (from the ML family).

The long-term goal is a statically typed language that stays lightweight to
write while keeping strong type safety.

---

## 2. Language Overview

### Functions

Types can be inferred, partially annotated, or fully explicit:

```text
fun factorial(n) {
    var r = 1;
    for var i = 1; i <= n; ++i {
        r = r * i;
    }
    return r;
}

fun factorial_count(n : int, count : ref int) -> int {
    var r : int = 1;
    count = 0;
    for var i = 1; i <= n; ++i {
        r = r * i;
        ++count;
    }
    return r;
}
```

### Structs, Inheritance, and Methods

```text
type point = struct {
    x: int;
    y: int;
}

type colored_point = struct : point {
    color: string;
}

fun point::module2() {
    return x*x + y*y;
}
```

Method bodies reach their struct's fields directly, inherited ones included, with
no explicit `self` or `this`. A field redeclared in a child shadows the parent's.

### Interfaces

Interfaces declare method signatures. Satisfaction is structural — any type
whose methods unify with the interface's row is compatible:

```text
type stringer = interface {
    string() -> string;
}

fun print_obj(x : stringer) {
    var s = x.string();
    sys::print(s);
}
```

### Tuples

Functions return multiple values, and callers destructure them:

```text
fun euclid(a, b) {
    return a/b, a%b;
}

var q, r = euclid(17, 5);
```

### Control Flow

```text
// C-style for loop (no parentheses around the header)
for var i = 0; i < n; ++i {
    // ...
}

// Range-based for loop
for var x = range(v) {
    total += x;
}

// If / else
if count == 0 {
    return 0;
} else {
    return total / count;
}
```

`continue` and `break` work inside loops.

For the full grammar see [`src/parser.y`](../src/parser.y).

---

## 3. Compiler Pipeline

```text
.dub ──> Lexer ──> Parser ──> AST ──┬──> Resolver ──> Type Checker ──┬──> Printer ──> stdout
         (Flex)    (Bison)          │                               │
                                    └───────────────────────────────┘
                                                --no-check
```

| Stage                                   | Role                                                                                                                                |
|-----------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------|
| **Lexer** (`src/lexer.l`)               | Tokenizes the source. Handles keywords, operators, literals, and `//` plus `/* */` comments.                                        |
| **Parser** (`src/parser.y`)             | Bison grammar building a tree of AST nodes. The root `program_node` lands in a global variable. Every node records its source line. |
| **AST** (`src/ast.h`)                   | Node hierarchy rooted at `ast_node`. Every node implements `accept(visitor&)`.                                                      |
| **Resolver** (`src/resolver.h`)         | Registers type names, their members, and function signatures over three passes (see below).                                         |
| **Type Checker** (`src/type_checker.h`) | Hindley-Milner inference over the resolved AST. Infers expression types, checks bodies, reports mismatches as warnings.             |
| **Printer** (`src/printer.h`)           | Prints the AST back as dubsar source. Roundtrip tests use it to verify parse-print stability.                                       |

`--no-check` skips the resolver and type checker, which is useful when working on
syntax alone.

### Resolver Passes

The resolver walks the top-level declarations three times, so no diagnostic and
no lookup depends on declaration order.

| Pass             | Work                                                           |
|------------------|----------------------------------------------------------------|
| `register_names` | Registers every type name with empty field and method rows     |
| `fill_types`     | Fills struct fields and interface method signatures            |
| `link`           | Links struct parents, registers function and method signatures |

Splitting `fill_types` from `link` is what makes forward references work in both
directions: a method may be declared before its type, and a struct may inherit
from a type declared later in the file.

---

## 4. Type System Design

The type system lives in three files: [`src/types.h`](../src/types.h) (type IR),
[`src/unify.h`](../src/unify.h) (unification engine), and
[`src/type_checker.h`](../src/type_checker.h) (inference visitor).

### Hindley-Milner Inference

Every unannotated variable or parameter starts as a **fresh type variable**. The
type checker walks the AST and emits **unification constraints** — passing `x` to
a function expecting `int` unifies `x`'s variable with `int`.

Unification uses **union-find** with path splitting. An **occurs check** rejects
infinite types such as `α = list<α>`.

`unify` normalises the two symmetric cases first: it resolves both sides, moves
any type variable to the left, and rejects a kind mismatch. What remains is a
same-kind structural comparison, one case per `type_kind`.

### Row Types

Structs and interfaces share one representation: **row types** (`row_type_t`),
extensible records of the form `{ label₁: T₁, label₂: T₂, ... | tail }`.

- **Structs are closed rows** — the tail is `nullptr`, so no extra fields fit.
- **Interfaces are open rows** — the tail is a fresh type variable, so any type
  with *at least* those methods satisfies the interface.

Interface satisfaction then falls out of **Remy-style row unification**: passing
a struct where an interface is expected matches the required labels and binds the
open tail to the rest. No `implements` declaration is needed.

An open tail absorbs whatever labels the other row holds in excess. A closed row
cannot, so excess facing a closed row is a type error naming the extra labels.

### Inheritance

`type_info` stores a struct's own fields plus a pointer to its parent, and field
lookup walks that chain. Flattening the parent's fields into the child would
duplicate the data and make the result depend on declaration order.

The resolver rejects inheritance cycles when it links parents, which is what
keeps the lookup walk finite.

### Let-Polymorphism

Top-level functions are **generalized** after inference: type variables that the
surrounding environment does not constrain become universally quantified. So:

```text
fun id(x) { return x; }
```

infers `∀α. α → α`. Each call site **instantiates** the scheme with fresh
variables, letting `id(42)` and `id("hello")` coexist.

Generalization happens only at top-level function boundaries. Local variables
stay monomorphic.

### `ref` Is Calling Convention, Not a Type

A parameter declared `p : ref int` has type `int`, not `ref<int>`. The `ref`
annotation lives on the parameter node as an `is_ref` flag and affects how the
argument is passed; the type system sees the underlying type only. This keeps the
type IR free of reference constructors and implicit dereferencing rules.

### Expression Types in a Side Table

The type checker stores inferred types in an
`unordered_map<const ast_node*, type_ptr>` rather than a field on every node.
The AST hierarchy therefore stays unchanged, the printer and the roundtrip tests
are unaffected by the semantic pass, and 28+ node classes need no edits.

### Permissive Inference

dubsar has no standard library, so calling an undefined function or referencing
an undefined variable yields a **fresh type variable** instead of an error. The
type checker can then make progress on code that calls external functions.
Resolution errors stay hard errors.

---

## 5. Key Design Decisions

| Decision                                                     | Rationale                                                                                                                                     |
|--------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------|
| **Parenthesize all binary ops in the printer**               | Guarantees roundtrip stability. Without explicit parens, any precedence difference between grammar and printer would change the second parse. |
| **Types are opaque strings in the AST**                      | The parser stores annotations raw (`"vector<int>"`); `parse_type_string()` converts them during the semantic pass. Keeps the grammar simpler. |
| **Resolution errors are hard, type mismatches are warnings** | A duplicate name or unresolvable parent is unrecoverable. A mismatch may be a false positive under permissive inference, so output continues. |
| **Three-pass name resolution**                               | Types are named, then filled, then linked, so declaration order never affects results or diagnostics.                                         |
| **`%destructor` rules in the Bison grammar**                 | One rule per union type tag `delete`s the raw pointer, so error recovery does not leak the values it discards.                                |
| **Visitor pattern for all passes**                           | Resolver, type checker, and printer are all `visitor` subclasses. A new pass means a new visitor, not an AST change.                          |
| **One forward-declaration header (`src/ast_fwd.h`)**         | `visitor.h` and the generated `parser.hpp` both include it, so the AST class list is maintained in one place.                                 |

---

## 6. What's Missing

| Area                    | State                                                                                          |
|-------------------------|------------------------------------------------------------------------------------------------|
| Code generation         | No backend. An LLVM or C backend is the natural next step.                                     |
| Standard library        | No built-ins (`print`, `len`); the type checker uses fresh variables as placeholders.          |
| Pattern matching        | Not in the grammar; would complement structural typing.                                        |
| Modules / imports       | Single file only. Qualified calls (`ns::func`) parse but have no module system behind them.    |
| Parser diagnostics      | `yyerror` only. Source spans and suggestions are a goal.                                       |
| Method inheritance      | Fields are inherited, methods are not; a child cannot yet reuse or override a parent's method. |
| For-range element types | The loop variable stays a free type variable because `range()` has no declared signature.      |
| Closures / lambdas      | Not supported.                                                                                 |
| Enums / algebraic types | Not supported; a natural complement to pattern matching.                                       |
| Const / immutability    | No `const` qualifier.                                                                          |
