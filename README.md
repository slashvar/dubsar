# dubsar

A compiler frontend (work in progress) for the **dubsar** programming language.
It lexes, parses, builds an AST, resolves names, infers types, and pretty-prints
the result. Code generation is not implemented.

See [`docs/DESIGN.md`](docs/DESIGN.md) for the architecture and type system.

## Language features

- Functions with inferred or explicit type annotations
- `ref` parameters (pass-by-reference)
- `var` declarations with optional type annotations (`var x = 10;`, `var x: int = 10;`)
- Tuple variables and assignments (`var x, y = f();`, `return a, b;`, `v[i], v[j] = v[j], v[i];`)
- Init-list expressions (`var v: vector<int> = {};`)
- `type` declarations for structs with optional single inheritance
- `type` declarations for interfaces with method signatures
- Methods declared as `fun TypeName::methodName()`
- Member calls (`obj.method(args)`) and qualified calls (`ns::func(args)`)
- Generic types (`vector<int>`, `vector<byte>`, …)
- Sized integers (`integer<64>` signed, `integer<+64>` unsigned)
- C-style `for` loops: `for var i = 0; i < n; ++i { ... }` and `for expr; expr; expr { ... }`
- Range-based `for` loops: `for var item = range(collection) { ... }`
- `if`/`else` statements
- `continue` and `break`
- Arithmetic operators (including `%`), comparison, and logical operators (`&&`, `||`, `!`)
- Compound assignment operators (`+=`, `-=`, `*=`, `/=`)
- Pre/post increment/decrement (`++`/`--`)
- String literals and the built-in `string` type

See `examples/` for sample source files.

## Requirements

| Tool         | Notes                                                                                                                                            |
|--------------|--------------------------------------------------------------------------------------------------------------------------------------------------|
| Clang        | Required; the build errors out on any other compiler                                                                                             |
| Flex         | Lexer generation                                                                                                                                 |
| Bison ≥ 3.0  | Parser generation (`%destructor` type tags and `%empty` need Bison 3+; the macOS system Bison is too old — `brew install bison`, Meson finds it) |
| Meson ≥ 1.0  | Build system                                                                                                                                     |
| Ninja        | Build backend                                                                                                                                    |
| Python 3     | Test runner                                                                                                                                      |
| clang-format | Optional, for the `format` target                                                                                                                |
| clang-tidy   | Optional, for the `tidy` target                                                                                                                  |

[argparse](https://github.com/p-ranav/argparse) 3.2 is header-only and Meson
fetches it from WrapDB automatically.

## Build

```bash
meson setup build   # first time, or after editing meson.build
ninja -C build
```

## Usage

The binary lands at `build/src/dubsar`. It reads one `.dub` file, writes the
pretty-printed program to stdout, and writes diagnostics to stderr.

```bash
build/src/dubsar examples/example.dub
build/src/dubsar --no-check examples/example.dub   # parse and print only
```

| Flag         | Effect                              |
|--------------|-------------------------------------|
| `--no-check` | Skips the resolver and type checker |
| `--help`     | Prints usage                        |

Resolution errors (duplicate names, bad inheritance) exit 1. Type mismatches are
warnings and still produce output.

## Tests

```bash
meson test -C build
meson test -C build --print-errorlogs   # show output from failures
```

| Category    | Checks                                                                   |
|-------------|--------------------------------------------------------------------------|
| `roundtrip` | Parse a fixture, print it, re-parse, and compare the two printed outputs |
| `error`     | Malformed input exits non-zero                                           |

## Build targets

| Target   | Command                 | Effect                                                 |
|----------|-------------------------|--------------------------------------------------------|
| `format` | `ninja -C build format` | `clang-format -i` over the hand-written C++            |
| `tidy`   | `ninja -C build tidy`   | `clang-tidy` over the same files; skips generated code |
| `clean`  | `ninja -C build clean`  | Removes artifacts, keeps the build configured          |

To reset fully after editing `meson.build`:

```bash
rm -rf build && meson setup build
```
