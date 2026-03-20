# dubsar Design Document

This document orients contributors on the language goals, compiler architecture,
type system rationale, and key design decisions behind dubsar.

For exhaustive field-level detail (AST node hierarchy, every keyword, operator
precedence, etc.) see [`CLAUDE.md`](../CLAUDE.md). For build instructions see
[`README.md`](../README.md).

---

## 1. Introduction & Goals

**dubsar** is a work-in-progress compiler frontend for a custom programming
language of the same name. The project currently covers lexing, parsing, AST
construction, name resolution, type inference, and pretty-printing. Code
generation is not yet implemented.

Design influences:

- **OCaml / Hindley-Milner** — global type inference with let-polymorphism;
  programmers can omit type annotations and the compiler infers them.
- **Go-like syntax** — braces for blocks, no parentheses around `for`/`if`
  conditions, short variable declarations with `var`.
- **Structural typing via row types** — interfaces are satisfied structurally
  (like Go), and the mechanism is row-type unification (from the ML family).

The long-term goal is a statically typed language that feels lightweight to
write (minimal annotations) while providing strong type safety.

---

## 2. Language Overview

### Functions

Types can be fully inferred, partially annotated, or fully explicit:

```
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

```
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

Methods access their struct's fields directly (no explicit `self`/`this`).

### Interfaces

Interfaces declare method signatures. Satisfaction is structural — any type
whose methods unify with the interface's row is compatible:

```
type stringer = interface {
    string() -> string;
}

fun print_obj(x : stringer) {
    var s = x.string();
    sys::print(s);
}
```

### Tuples

Functions can return multiple values, and callers can destructure them:

```
fun euclid(a, b) {
    return a/b, a%b;
}

var q, r = euclid(17, 5);
```

### Control Flow

```
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

`continue` and `break` are supported inside loops.

For the full grammar see [`src/parser.y`](../src/parser.y).

---

## 3. Compiler Pipeline

```
.dub ──> Lexer ──> Parser ──> AST ──┬──> Resolver ──> Type Checker ──┬──> Printer ──> stdout
         (Flex)    (Bison)          │    (pass 1)      (pass 2)      │
                                    │                                │
                                    └────────────────────────────────┘
                                                --no-check
```

| Stage | Role |
|-------|------|
| **Lexer** (`src/lexer.l`) | Tokenizes the source file. Handles keywords, operators, literals, and comments (`//`, `/* */`). |
| **Parser** (`src/parser.y`) | Bison grammar that builds a tree of AST nodes. The root is a `program_node` stored in a global variable. Every node records its source line number for diagnostics. |
| **AST** (`src/ast.h`) | Node class hierarchy rooted at `ast_node`. All nodes implement `accept(visitor&)` for the visitor pattern. |
| **Resolver** (`src/resolver.h`) | Name resolution in two sub-passes: (1) register all type names, (2) fill in struct fields, interface methods, function signatures, and resolve inheritance. This two-pass approach allows forward references. |
| **Type Checker** (`src/type_checker.h`) | Hindley-Milner type inference over the resolved AST. Infers expression types, checks function/method bodies, and reports mismatches as warnings. |
| **Printer** (`src/printer.h`) | Pretty-prints the AST back to dubsar source. Used by roundtrip tests to verify parse-print stability. |

The `--no-check` flag skips the Resolver and Type Checker, running only
parse + print. This is useful for working on syntax without triggering
semantic errors.

---

## 4. Type System Design

This is the most novel part of the project. The type system lives in three
files: [`src/types.h`](../src/types.h) (type IR),
[`src/unify.h`](../src/unify.h) (unification engine), and
[`src/type_checker.h`](../src/type_checker.h) (inference visitor).

### Hindley-Milner Inference

Every unannotated variable or parameter starts as a **fresh type variable**.
The type checker walks the AST and emits **unification constraints** — for
example, if `x` is passed to a function expecting `int`, then `x`'s type
variable is unified with `int`.

Unification uses **union-find** with path compression. An **occurs check**
prevents infinite types (e.g. `α = list<α>`).

### Row Types

The key insight: structs and interfaces are both represented as **row types**
(`row_type_t`), which are extensible records of the form
`{ label₁: T₁, label₂: T₂, ... | tail }`.

- **Structs are closed rows** — the tail is `nullptr`, meaning no extra fields
  are allowed.
- **Interfaces are open rows** — the tail is a fresh type variable, meaning
  any type with *at least* those methods can satisfy the interface.

Interface satisfaction falls out naturally from **Remy-style row unification**:
when a struct is passed where an interface is expected, the unifier matches
the required labels and binds the open tail to the remaining fields. No
explicit `implements` declaration is needed.

### Let-Polymorphism

Top-level functions are **generalized** after inference: any type variables
that are not constrained by the surrounding environment become universally
quantified. For example:

```
fun id(x) { return x; }
```

infers the type `∀α. α → α`. Each call site **instantiates** the scheme with
fresh variables, so `id(42)` and `id("hello")` can coexist.

Generalization happens at top-level function boundaries. Local variables are
not generalized (monomorphism restriction within function bodies).

### `ref` Is Calling Convention, Not a Type

A parameter declared `p : ref int` has type `int`, not `ref<int>`. The `ref`
annotation is stored on the parameter node (`is_ref` flag) and affects how the
argument is passed, but the type system sees only the underlying type. This
keeps the type IR simpler and avoids needing reference-type constructors or
implicit dereferencing rules.

### Expression Types in a Side Table

Rather than adding a `type_ptr` field to every AST node class, the type
checker stores inferred types in an external map
(`unordered_map<const ast_node*, type_ptr>`). This keeps the AST node
hierarchy unchanged — the printer and roundtrip tests are completely
unaffected by the semantic pass, and the 28+ node classes don't need
modification.

### Permissive Inference

Because dubsar has no standard library yet, calling an undefined function or
referencing an undefined variable produces a **fresh type variable** instead
of a hard error. This allows the type checker to make progress on realistic
code that calls external functions. Resolution errors (duplicate type names,
etc.) are still hard errors.

---

## 5. Key Design Decisions

| Decision | Rationale |
|----------|-----------|
| **Parenthesize all binary ops in the printer** | Guarantees roundtrip stability (parse → print → parse → compare). Without explicit parens, precedence differences between the grammar and the printer could cause the second parse to produce a different AST. |
| **Types are opaque strings in the AST** | The parser stores type annotations as raw strings (e.g. `"vector<int>"`). Parsing them into the type IR (`parse_type_string()`) happens later in the semantic pass. This keeps the Bison grammar simpler. |
| **Resolution errors are hard, type mismatches are warnings** | A duplicate type name or unresolvable inheritance is unrecoverable. A type mismatch might be a false positive (given the permissive inference strategy), so it is reported but does not block output. |
| **Two-pass name resolution** | Pass 1 registers all type names; pass 2 fills in details. This allows types to reference each other regardless of declaration order (forward references). |
| **`%destructor` rules in the Bison grammar** | Each union type tag has a destructor rule that `delete`s the raw pointer. This prevents memory leaks when Bison discards tokens during error recovery. |
| **Visitor pattern for all passes** | Resolver, type checker, and printer are all `visitor` subclasses. Adding a new pass means writing a new visitor without modifying AST node classes. |

---

## 6. What's Missing / Next Steps

dubsar is a work in progress. Major areas not yet implemented:

- **Code generation** — no backend; the compiler currently only parses and
  type-checks. An LLVM or C backend would be the natural next step.
- **Standard library** — no built-in functions (`print`, `len`, etc.); the
  type checker uses fresh type vars as a placeholder.
- **Pattern matching** — not yet in the grammar; would complement the
  structural typing well.
- **Modules / imports** — currently single-file only; qualified calls
  (`ns::func`) exist syntactically but there is no module system behind them.
- **Error recovery & diagnostics** — parser error messages are basic
  (`yyerror`); richer diagnostics with source spans and suggestions are a goal.
- **Closures / lambdas** — not yet supported.
- **Enums / algebraic data types** — a natural complement to pattern matching.
- **Const / immutability** — no `const` qualifier yet.
