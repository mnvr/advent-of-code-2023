Haskell and Swift¹ solutions to [Advent of Code 2023](https://adventofcode.com/2023).

Each solution is in a self-contained file with zero dependencies that can be
run directly by the stock Haskell and Swift compilers. For example:

    cat examples/18 | runghc 18.hs
    cat examples/18 | swift 18.swift

More details about running, including some convenient `make` invocations, are
described below.

<small>¹ The primary solutions are is Haskell. But for some problems, I've also
done variations using other means and languages, mostly Swift.</small>

## Running

Install [Haskell using GHCup](https://www.haskell.org/ghcup/), or
[Swift](https://swift.org), depending on which solutions you want to run.

As mentioned, each .hs or .swift file is a complete, zero dependency file that
reads its input from STDIN, and can be run by the stock compilers, e.g.

    cat examples/18 | runghc 18.hs
    cat examples/18 | swift 18.swift

However, my own workflow is slightly different. I've created a bunch of rules in
the `Makefile` that take the latest (the most recently modified) source code
file, and then automatically run it, passing the day's example as input. So I
just edit a file, and run

    make

That's it. If I edit a different file, its modification time will change, so I
can just run `make` again and that will get run instead.

There are several other make rules (again, you don't need to use these to run
the solutions, these are just tailored for my own workflow):

-   `make test` – run on the actual input, and compare against the expected
    answers. The actual inputs should be in `inputs/xx`, and the expected
    answers in `answers/xx-a` and `answers/xx-b` (the `inputs` and `answers`
    directories are gitignored).
-   `make o` - same as `make test`, but first compile the file using `ghc` or
    `swiftc` with optimizations enabled. There's also a `make o2` variant of
    this for Haskell that uses the `-O2` level.
-   `make verify` - Run `make test`, but for all days so far, in reverse. Print
    nice and pretty stats about all the days too.
-   `make clean` - Clean temporary files (written to `out/`).

To run the command on a specific day's program without needing to edit it, just
`touch` it so that it has the latest modification time - subsequent make
invocations will pick it. So for example, if I wanted to run day 18's Haskell
solution, I can

    touch 18.hs
    make

This works with the example inputs too. Some days have multiple examples, so to
use one of them specifically, just `touch` it beforehand.

All these make invocations are wrappers around the basic pattern of:

    cat somefile | runghc xx.hs
    cat somefile | swift xx.swift

But feel free to peek into the `Makefile` itself. I had as much fun writing this
Makefile as I had writing some of the solutions.

Some of the solutions are done in shell scripts. To run these, the filename
needs to be passed to the script instead of passing the input via stdin²:

    ./xx.sh somefile

<small>² (these were done differently because reading STDIN in a shell script is
not straightforward, and would've otherwise shadowed the solution
itself)</small> .
