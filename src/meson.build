# Generate parser from bison (output .cpp so meson uses C++ compiler)
parser_cpp = custom_target('parser_cpp',
    output: 'parser.cpp',
    command: ['/usr/bin/bison', '-d', '-o', '@OUTPUT@', '@INPUT@'],
    input: 'parser.y'
)

# Generate lexer from flex (output .cpp so meson uses C++ compiler)
lexer_cpp = custom_target('lexer_cpp',
    output: 'lexer.cpp',
    command: ['/usr/bin/flex', '-o', '@OUTPUT@', '@INPUT@'],
    input: 'lexer.l'
)

# Generated sources are compiled without -Werror to silence warnings in
# flex/bison output that we cannot control.
generated_lib = static_library('generated',
    parser_cpp,
    lexer_cpp,
    include_directories: include_directories('.'),
    cpp_args: ['-Wno-error', '-w'],
)

dubsar = executable('dubsar',
    'main.cpp',
    'ast.cpp',
    'printer.cpp',
    include_directories: include_directories('.'),
    link_with: generated_lib,
    install: false
)

# ── Tests ────────────────────────────────────────────────────────────────────

python3    = find_program('python3')
run_test   = meson.project_source_root() / 'tests' / 'run_test.py'
valid_dir  = meson.project_source_root() / 'tests' / 'fixtures' / 'valid'
invalid_dir = meson.project_source_root() / 'tests' / 'fixtures' / 'invalid'

# Round-trip tests: parse → print → parse → print, check the two prints match.
valid_cases = [
    'function_inferred',
    'function_ref_params',
    'function_typed',
    'for_loop',
    'operators',
    'struct',
    'struct_inherit',
    'methods',
    'string_type',
    'interface',
    'example2',
]

foreach name : valid_cases
    test('roundtrip-' + name, python3,
         args: [run_test, 'roundtrip', dubsar.full_path(),
                valid_dir / (name + '.dub')],
         depends: dubsar)
endforeach

# The bundled example is also round-trip tested.
test('roundtrip-full-example', python3,
     args: [run_test, 'roundtrip', dubsar.full_path(),
            meson.project_source_root() / 'examples' / 'example.dub'],
     depends: dubsar)

# Error tests: the binary must exit non-zero.
invalid_cases = [
    'missing_brace',
    'missing_semicolon',
    'unknown_token',
    'incomplete_fun',
]

foreach name : invalid_cases
    test('error-' + name, python3,
         args: [run_test, 'error', dubsar.full_path(),
                invalid_dir / (name + '.dub')],
         depends: dubsar)
endforeach
