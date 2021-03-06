# Python compiler

Python 3 implementation using LLVM.

# Build instructions

This project is written in [Rust](https://www.rust-lang.org) and depends on
[LLVM 11](https://llvm.org). To compile file `test.py`:

## macOS

Install Rust using [rustup](https://rustup.rs).
LLVM can be installed using [Homebrew](https://brew.sh):

```shell
brew install llvm@11
```

Build compiler:

```shell
export LLVM_SYS_110_PREFIX="$(brew --prefix)/opt/llvm@11/"

cargo build --release
```

Build final executable:

```shell
export PATH="$(brew --prefix)/opt/llvm@11/bin:$PATH"

target/release/python --emit=bc test.py
llc -filetype=obj test.bc
clang test.o -o test -L target/release/ -lpython_core
```

# License

MIT.

Copyright (c) 2021 Dominik Miedziński.
