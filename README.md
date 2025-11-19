# `-fportcosmo`: patching `clang` to build C software with Cosmopolitan Libc

This patch is the LLVM equivalent of my [gcc patch][gccpatch] to simplify
building software with Cosmopolitan Libc. It may not be necessary to patch
compilers now that [most constants in Cosmopolitan Libc are compile-time
again][cosmoconst], but I think this was still a useful exercise, and now I know
something about LLVM internals. You can clone the repo and build `clang` as
usual to get this feature.

## How does it work?

`-fportcosmo` works by rewriting the AST. Think of it as a "context-sensitive
LISP-y `defmacro` error handler", that activates before `clang` can trigger a
`switch case is not constant` or `initializer element is not constant` error.

* the error is triggered because `clang` expects the case or initializer to be a
  compile-time constant, which it isn't
* this patch ignores the error when `-fportcosmo`, and immediately rewrites the
  necessary statements before further analysis occurs in the `Sema` calls.
* this patch walks through the AST rewriting necessary subtrees: `switch`
  statements are rewritten into `if` statements (with appropriate `goto`s to
  handle fallthroughs/`break` statements, and `struct` initializers are appended
  with one-time initializations (via `__attribute__((constructor))` for globals
  or an `if` for locals). No other part of the code being compiled is affected.

For an extended explanation, refer to the three READMEs [with my
gcc plugin][plugin].  Note that this patch currently does not work with: 

* `constexpr` initializations
* `enum`s (rewrite to `#define`s instead), or 
* wacky situtations where `SIGTERM` is used as an array index within an
  initializer

Finally, remember this patch is just a convenience -- you could always rewrite
the `switch` statements and `struct` initializers manually.

[gccpatch]: github.com/ahgamut/gcc/tree/portcosmo-14.1
[cosmoconst]: https://github.com/jart/cosmopolitan/commit/5ddb5c2adad79407fb800fce47f389611f90a511
[plugin]: https://github.com/ahgamut/cosmo-gcc-plugin

# The LLVM Compiler Infrastructure

[![OpenSSF Scorecard](https://api.securityscorecards.dev/projects/github.com/llvm/llvm-project/badge)](https://securityscorecards.dev/viewer/?uri=github.com/llvm/llvm-project)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/8273/badge)](https://www.bestpractices.dev/projects/8273)
[![libc++](https://github.com/llvm/llvm-project/actions/workflows/libcxx-build-and-test.yaml/badge.svg?branch=main&event=schedule)](https://github.com/llvm/llvm-project/actions/workflows/libcxx-build-and-test.yaml?query=event%3Aschedule)

Welcome to the LLVM project!

This repository contains the source code for LLVM, a toolkit for the
construction of highly optimized compilers, optimizers, and run-time
environments.

The LLVM project has multiple components. The core of the project is
itself called "LLVM". This contains all of the tools, libraries, and header
files needed to process intermediate representations and convert them into
object files. Tools include an assembler, disassembler, bitcode analyzer, and
bitcode optimizer.

C-like languages use the [Clang](https://clang.llvm.org/) frontend. This
component compiles C, C++, Objective-C, and Objective-C++ code into LLVM bitcode
-- and from there into object files, using LLVM.

Other components include:
the [libc++ C++ standard library](https://libcxx.llvm.org),
the [LLD linker](https://lld.llvm.org), and more.

## Getting the Source Code and Building LLVM

Consult the
[Getting Started with LLVM](https://llvm.org/docs/GettingStarted.html#getting-the-source-code-and-building-llvm)
page for information on building and running LLVM.

For information on how to contribute to the LLVM project, please take a look at
the [Contributing to LLVM](https://llvm.org/docs/Contributing.html) guide.

## Getting in touch

Join the [LLVM Discourse forums](https://discourse.llvm.org/), [Discord
chat](https://discord.gg/xS7Z362),
[LLVM Office Hours](https://llvm.org/docs/GettingInvolved.html#office-hours) or
[Regular sync-ups](https://llvm.org/docs/GettingInvolved.html#online-sync-ups).

The LLVM project has adopted a [code of conduct](https://llvm.org/docs/CodeOfConduct.html) for
participants to all modes of communication within the project.
