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

Then, to run the latest solution on the latest of its example inputs, use

    make

Other commands:

-   `make test` – run on the actual input, and compare against the expected
    answers. The actual inputs should be in `input/xx`, and the expected answers
    in `answers/xx-a` and `answers/xx-b`.
-   `make o` - same as `make test`, but first compile the file using `ghc` or
    `swiftc` with optimizations enabled. There's also a `make o2` variant of
    this for Haskell that uses the `-O2` level.
-   `make verify` - Run `make test`, but for all days so far, in reverse. Print
    nice and pretty stats about all the days too.
-   `make clean` - Clean temporary files (written to `out/`).

To run the command on a specific day's program, just `touch` it so that it has
the latest modification time - subsequent make invocations will pick it. So for
example, if I wanted to run day 18, I can

    touch 18.swift # or touch 18.hs, depending on which I wish to run
    make

All of make invocations are wrappers around the basic pattern of

    cat somefile | runghc xx.hs

and

    cat somefile | swift xx.swift

Some of the solutions are done in shell scripts. To run these, the filename
needs to be passed to the script instead of passing the input via stdin:

    ./xx.sh somefile
