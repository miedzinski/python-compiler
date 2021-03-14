# Python compiler

Python 3 implementation using LLVM.

# Build instructions

This project is written in [Rust](https://www.rust-lang.org) and depends on
[LLVM 11](https://llvm.org) and [Boehm GC](https://www.hboehm.info/gc/).
To compile file `test.py`:

## macOS

Install Rust using [rustup](https://rustup.rs).
LLVM and Boehm GC can be installed using [Homebrew](https://brew.sh):

```shell
brew install llvm@11 libgc
```

Build compiler:

```shell
LLVM_SYS_110_PREFIX="$(brew --prefix)/opt/llvm@11/" cargo build --release
```

Build final executable:

```shell
target/release/python --emit=bc test.py
$(brew --prefix)/opt/llvm@11/bin/llc -filetype=obj test.bc
clang test.o -o test -Ltarget/release/ -lpython_core -lgc
```

# License

MIT.

Copyright (c) 2021 Dominik Miedziński.
