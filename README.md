# dubsar

A compiler frontend (work in progress) for the **dubsar** programming language.
Currently implements lexing, parsing, AST construction, and pretty-printing.

## Language features

- Functions with inferred or explicit type annotations
- `ref` parameters (pass-by-reference)
- `var` declarations with optional type annotations (`var x = 10;`, `var x: int = 10;`)
- Tuple variables and assignments (`var x, y = f();`, `return a, b;`)
- Init-list expressions (`var v: vector<int> = {};`)
- `type` declarations for structs with optional single inheritance
- `type` declarations for interfaces with method signatures
- Methods declared as `fun TypeName::methodName()`
- Member calls (`obj.method(args)`) and qualified calls (`ns::func(args)`)
- Generic types (`vector<int>`, `vector<byte>`, etc.)
- C-style `for` loops: `for var i = 0; i < n; ++i { ... }`
- Range-based `for` loops: `for var item = range(collection) { ... }`
- `if`/`else` statements
- `continue`
- Arithmetic operators (including `%`), comparison, and logical operators (`&&`, `||`, `!`)
- Compound assignment operators (`+=`, `-=`, `*=`, `/=`)
- Pre/post increment/decrement (`++`/`--`)
- String literals and the built-in `string` type

See `examples/` for sample source files.

## Requirements

| Tool | Notes |
|------|-------|
| Clang | Required (GCC is not supported) |
| Flex | Lexer generation |
| Bison | Parser generation |
| Meson ≥ 1.0 | Build system |
| Ninja | Build backend |
| Python 3 | Test runner |
| clang-format | Optional, for the `format` target |
| clang-tidy | Optional, for the `tidy` target |

## Build

```bash
# First-time setup
meson setup build

# Compile
ninja -C build
```

The binary is produced at `build/src/dubsar`. It reads a `.dub` source file and
prints the parsed AST back as formatted dubsar source code:

```bash
build/src/dubsar examples/example.dub
```

## Tests

```bash
meson test -C build
```

Tests are registered in two categories:

- **roundtrip** — parses a fixture, prints it, re-parses the output, and checks
  that both printed outputs are identical (verifies printer stability).
- **error** — checks that malformed inputs are rejected with a non-zero exit code.

To see output from failing tests:

```bash
meson test -C build --print-errorlogs
```

## Clean

```bash
ninja -C build clean
```

This removes compiled artifacts while keeping the build configured. To fully
reset (e.g. after changing `meson.build`), remove the directory and re-run setup:

```bash
rm -rf build && meson setup build
```

## Format

```bash
ninja -C build format
```

Runs `clang-format -i` on all hand-written C++ sources using the `.clang-format`
config (Google style, 4-space indent, 100-column limit).

## Lint

```bash
ninja -C build tidy
```

Runs `clang-tidy` on all hand-written C++ sources using the `.clang-tidy` config.
Requires a configured build directory so that `compile_commands.json` is available.
Generated Flex/Bison sources are excluded.
