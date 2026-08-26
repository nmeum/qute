<!--
SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>

SPDX-License-Identifier: GPL-3.0-only
-->

## Qute

Qute is a work-in-progress software analysis framework built around the [QBE] intermediate language.
Architecturally, it is composed of [several][Hackage qute-syntax] [modular][Hackage qute] [Haskell][Hackage qute-symex] [libraries][Hackage qute-cli], which enable the implementation of [static][static software analysis] and [dynamic][dynamic software analysis] software analysis techniques for QBE programs.
The foundation of the framework is a formal (yet executable) description of the QBE specification, which is implemented using an [abstract monad].
Currently, Qute focuses primarily on [dynamic software analysis] through [symbolic execution].

For further details, see the **[Qute paper][ASYDE]**.

### Status

Qute is still in early stages of development, not all desired functionality has been implemented and bugs are to be expected in the existing functionality.
Nonetheless, it supports all relevant features of the [QBE specification][QBE v1.3] and is thus compatible with existing compiler frontends for QBE.
This includes the [cproc] and [SCC] (with an [upstreamed patch][SCC patch]) C compilers as well as the [Hare compiler][Hare].

### Architecture

The foundation of this project is a formal, yet executable, description of the QBE intermediate language.
At the time of writing, it targets [v1.3 of the QBE specification][QBE v1.3].
The syntax is specified using [literate Haskell][literate programming] and [parser combinators] in the [`qute-syntax`][Hackage qute-syntax] library.
The language semantics are expressed in a modular way by distinguishing abstract and actual semantics.
*Abstract semantics* of the QBE language are described in terms of a [`Simulator`] monad (i.e., an [abstract monad]).
This monad must then be instantiated, whereby *actual semantics* are specified.
Presently, the following instantiations are supported:

1. Concrete semantics, provided by [`Language.QBE.Simulator.Default.State`].
2. [Symbolic][symbolic execution] (specifically [concolic][concolic testing]) semantics through [`Language.QBE.Simulator.Concolic.State`].

The former is primarily useful for simulation of programs written in the QBE intermediate language.
The latter is intended for automated software testing using [symbolic execution] and—as demonstrated below—can be used to automatically generate test inputs.
Central to this end is the abstract description of QBE semantics that is provided, together with a concrete instantiation, by the [`qute`][Hackage qute] library.
Further, a symbolic instantiation is provided in a separate [`qute-symex`][Hackage qute-symex] library.

Additionally, executable programs are provided through [`qute-cli`][Hackage qute-cli].
These programs can be used directly from a shell, without interacting with the Haskell codebase.
Presently the following executable program components are available:

1. `qute`: A simulator for QBE programs built on top of the concrete semantics.
2. `qute-symex`: An automated software testing tool facilitating the symbolic semantics.

These program components operate directly on QBE input programs.

### Installation

All Haskell libraries provided in this repository are [available on Hackage][Hackage qute] and can hence be installed directly using [Cabal], the Haskell language package manager.
As an example, `qute-cli` can be installed using:

```
$ cabal install qute-cli
```

However, to obtain a compatible GHC toolchain and external software (such as constraint solvers), installation using [Guix] is recommended.
For example, in order to install the `qute-cli` component and the [Bitwuzla] solver using Guix:

```
$ guix time-machine -C .guix/channels.scm -- install -L .guix/modules/ qute-cli bitwuzla
```

Afterwards, if Guix is configured correctly, the aforementioned program components (`qute` and `qute-symex`) should be available in your `$PATH`.
Note that you can also install additional packages, for example, the [cproc][guix cproc] or [simple-cc][guix simple-cc] QBE-based C compilers this way.

### Demonstration

Qute shines when used as a library to implement custom static and dynamic analysis techniques based on QBE.
For details in this regard, refer to the [Hackage documentation][Hackage qute] of the provided Haskell libraries.
Additionally, the [`qute-cli`][Hackage qute-cli] package provides several program components which can be used without writing your own Haskell code.
The following subsections demonstrate the utilization of this program components.

#### Concrete Execution

Consider the following "Hello, World!" program:

```C
#include <stdio.h>

int main(void) {
	puts("Hello, World!");
	return 0;
}
```

In order to concretly execute this program using `qute`, we need to obtain an equivalent representation in QBE.
To this end, we can invoke the [cproc] compiler as follows:

```
$ cproc -emit-qbe hello.c
```

The resulting QBE file can then be executed with the concrete semantics using:

```
$ qute hello.qbe
Hello, World!
```

Note that `qute` is only able to invoke the `puts(3)` function because it intercepts its execution, providing a "simulated" version of it.
Presently, only a limited amount of standard library functions are intercepted in this way.
As such, interactions with the file system or more complex output functions (e.g. `printf(3)`) are currently not supported.

#### Symbolic Execution

[Symbolic execution][symbolic execution] is a dynamic software analysis technique that explores reachable program paths based on a symbolic input variable (i.e., an input source).
As an example, consider the following C program:

```C
#include <stdio.h>
#include <stddef.h>

// Convert a memory region to an unconstrained symbolic value. Like calloc(3), it can
// account for memory regions which store multiple elements (nelem) of a specific size
// (elsiz). The give name is used to identify the symbolic variable and must be unique.
extern void qute_make_symbolic(void *ptr, size_t nelem, size_t elsiz, const char *name);

int main(void) {
	int a;
	qute_make_symbolic(&a, 1, sizeof(a), "a");
	if (a == 42) {
		puts("you found the answer");
	} else {
		puts("not the answer");
	}

	return 0;
}
```

This program can be compiled using the QBE-based [cproc] C11 compiler as follows:

```
$ cproc -emit-qbe example.c
```

The resulting QBE representation (`example.qbe`) can be symbolically executed using `qute-symex`:

```
$ qute-symex --write-tests tests/ example.qbe
```

This will yield the following output:

```
not the answer
you found the answer

---
Amount of paths: 2
```

This indicates that we found two execution paths through our program based on the symbolic variable `a`.
Due to the `--write-tests` option, `qute-symex` will create a `tests/` directory that contains test inputs in the [ktest format][KLEE ktest], one for each execution path.
These files can be inspected with the `ktest-tool` from [KLEE] (which is included in the Guix development environment described below).
For example:

```
$ ktest-tool tests/test000002.ktest
ktest file : 'tests/test000002.ktest'
args       : ['example.qbe']
num objects: 1
object 0: name: 'a1'
object 0: size: 4
object 0: data: b'*\x00\x00\x00'
object 0: hex : 0x2a000000
object 0: int : 42
object 0: uint: 42
object 0: text: *...
```

This tells us that the second execution path, where the program prints `you found the answer`, was triggered with `a1 := 42`.
From these files, we can—for example—automatically [generate high-coverage test cases][KLEE OSDI].
In the future, it will be possible to replay selected `.ktest` files using `qute-cli`.

### Tutorials

More practical examples wrt. utilization of Qute and symbolic execution are available separately:

* [Executing Hare Programs using Qute](https://notes.8pit.net/notes/zwts.html)
* [Validating Hare’s Sort Module using Symbolic Execution](https://notes.8pit.net/notes/y7n8.html)
* [An Introduction to Automated Software Testing using Symbolic Execution](https://media.ccc.de/v/ho26-146-an-introduction-to-automated-software-testing-using-symbolic-execution) (uses [KLEE])

### Design Goals

This project is intentionally written in a simple subset of the [Haskell] programming language.
It should be usable by anyone with a basic Haskell background (e.g., as obtained by reading [Learn You a Haskell for Great Good!][learnyouahaskell]).
Further, the project should require minimal long-term maintenance and should also support older GHC versions.
Therefore, it uses the [GHC2021] language standard and avoids usage of additional language extensions.
Further, whenever possible, dependencies on external libraries that are [not bundled by GHC][GHC libraries] must be avoided.

### Development

Code should be formatted using [ormolu][ormolu github].
Git hooks performing several sanity checks, including ensuring the proper code formatting, are available.
These hooks can be enabled using:

	$ git config --local core.hooksPath .githooks

Further, a [Guix] environment for development purposes can be obtained using:

	$ guix time-machine -C .guix/channels.scm -- shell -L .guix/modules/ -m .guix/manifest.scm

### License

This project uses the [REUSE Specification] to indicated used software license.

[QBE]: https://c9x.me/compile/
[QBE vs LLVM]: https://c9x.me/compile/doc/llvm.html
[QBE v1.3]: https://c9x.me/compile/doc/il.html
[QBE jumps]: https://c9x.me/compile/doc/il.html#Jumps
[LLVM]: https://llvm.org/
[KLEE]: https://klee-se.org
[KLEE OSDI]: https://www.usenix.org/legacy/events/osdi08/tech/full_papers/cadar/cadar.pdf#page=9
[KLEE ktest]: https://klee-se.org/releases/docs/v3.1/tutorials/testing-function/#klee-generated-test-cases
[SCC]: https://www.simple-cc.org/
[SCC patch]: https://git.simple-cc.org/scc/commit/575a2d87cac49174b7f53aa6ac5f9186c2165697.html
[cproc]: https://sr.ht/~mcf/cproc/
[Hare]: https://harelang.org/
[Haskell]: https://haskell.org/
[GHC]: https://www.haskell.org/ghc/
[GHC2021]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/control.html#extension-GHC2021
[GHC libraries]: https://ghc.gitlab.haskell.org/ghc/doc/libraries/index.html
[learnyouahaskell]: https://learnyouahaskell.github.io/chapters.html
[libriscv]: https://github.com/agra-uni-bremen/libriscv
[ormolu github]: https://github.com/tweag/ormolu
[REUSE Specification]: https://reuse.software/spec-3.3/
[Guix]: https://guix.gnu.org
[symbolic execution]: https://en.wikipedia.org/wiki/Symbolic_execution
[concolic testing]: https://en.wikipedia.org/wiki/Concolic_testing
[literate programming]: https://en.wikipedia.org/wiki/Literate_programming
[parser combinators]: https://en.wikipedia.org/wiki/Parser_combinator
[abstract monad]: https://doi.org/10.1145/3607833
[Cabal]: https://www.haskell.org/cabal/
[Bitwuzla]: https://bitwuzla.github.io/
[guix cproc]: https://hpc.guix.info/package/cproc
[guix simple-cc]: https://hpc.guix.info/package/simple-cc
[Hackage qute-symex]: https://hackage.haskell.org/package/qute-symex
[Hackage qute-syntax]: https://hackage.haskell.org/package/qute-syntax
[Hackage qute-cli]: https://hackage.haskell.org/package/qute-cli
[Hackage qute]: https://hackage.haskell.org/package/qute
[dynamic software analysis]: https://en.wikipedia.org/wiki/Dynamic_program_analysis
[static software analysis]: https://en.wikipedia.org/wiki/Static_program_analysis
[ASYDE]: https://www.ibr.cs.tu-bs.de/vss/Publications/2026/tempel_26_qute.pdf
[`Simulator`]: https://hackage-content.haskell.org/package/qute/docs/Language-QBE-Simulator-State.html#t:Simulator
[`Language.QBE.Simulator.Default.State`]: https://hackage-content.haskell.org/package/qute/docs/Language-QBE-Simulator-Default-State.html
[`Language.QBE.Simulator.Concolic.State`]: https://hackage-content.haskell.org/package/qute-symex/docs/Language-QBE-Simulator-Concolic-State.html
