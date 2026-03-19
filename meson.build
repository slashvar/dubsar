project('dubsar', ['cpp', 'c'],
    version: '0.1.0',
    default_options: ['cpp_std=c++20']
)

# Find flex and bison.
# Bison >= 3.0 is required for %destructor type-tag syntax.
# On macOS the system Bison is 2.x; Homebrew installs a newer version as a
# keg-only package, so we add its stable opt path to the search dirs.
flex = find_program('flex')
bison = find_program(
    'bison',
    dirs: ['/opt/homebrew/opt/bison/bin', '/usr/local/opt/bison/bin'],
    version: '>= 3.0',
)

# Find clang compiler
cxx = meson.get_compiler('cpp')
if cxx.get_id() != 'clang'
    error('This project requires clang compiler')
endif

add_project_arguments('-Wextra', '-Werror', language: 'cpp')

# Build sources
subdir('src')

# ── clang-tidy target ────────────────────────────────────────────────────────

clang_tidy = find_program('clang-tidy', required: false)
if clang_tidy.found()
    tidy_sources = files(
        'src/ast.h', 'src/ast.cpp',
        'src/visitor.h',
        'src/printer.h', 'src/printer.cpp',
        'src/main.cpp',
    )
    tidy_args = [clang_tidy, '-p', meson.project_build_root()]
    if host_machine.system() == 'darwin'
        tidy_args += '--extra-arg=--sysroot=' + run_command('xcrun', '--show-sdk-path', check: true).stdout().strip()
    endif
    tidy_args += tidy_sources
    run_target('tidy', command: tidy_args)
endif

# ── clang-format target ───────────────────────────────────────────────────────

clang_format = find_program('clang-format', required: false)
if clang_format.found()
    fmt_sources = files(
        'src/ast.h', 'src/ast.cpp',
        'src/visitor.h',
        'src/printer.h', 'src/printer.cpp',
        'src/main.cpp',
    )
    run_target('format',
        command: [clang_format, '-i', fmt_sources],
    )
endif