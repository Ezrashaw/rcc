# rcc (Rust C Compiler)

This is my toy C compiler, I'm aiming for full C11 compliance (see [the standard](https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1548.pdf)).

## Feature list
- integer literals
- `return` statement
- *all* C arithmetic operators (with proper precedence), with the following exceptions:
    - assignment operators (as in `a += 5`) (WIP)
    - prefix/postfix increment/decrement operators (as in `i++`) (WIP)
    - comma operator (unknown semi-useless operator)
- local variables (only `int` type)
- `if` statements (including `else` and ternary expressions)
- "blocks" (with proper scoping rules)
- loops:
    - both `for` loops
    - `while` and `do while` loops
    - `break/continue`
- C pretty-printer (somewhat working)
- optimization layer (somewhat working)
    - constant folding
    - variable inlining
- IR ([Intermediate representation](https://en.wikipedia.org/wiki/Intermediate_representation)), allowing multiple backends:
    - x86_64
    - arm64 (aarch64)
    - LLVM IR (somewhat working)
- full testing (from https://github.com/nlsandler/write_a_c_compiler)
