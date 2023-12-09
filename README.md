Haskell¹ solutions to [Advent of Code 2023](https://adventofcode.com/2023).

<small>¹ The primary solutions are is Haskell. But for some problems, I've also
done variations using other means and languages. </small>

## Running

Install [Haskell using GHCup](https://www.haskell.org/ghcup/). Then, to run the
latest solution on the latest of its example inputs, use

    make

Other commands:

-   `make test` – run on the actual input, and compare against the expected
    answers.
-   `make o` - same as `make test`, but first compile the .hs file using GHC
    with optimizations enabled. There's also a `make o2` variant of this.
-   `make verify` - Run `make test`, but for all days so far, in reverse.
-   `make clean` - Clean temporary files (written to `out/`).

All of these are wrappers around the basic pattern of

    cat somefile | runghc xx.hs

Some of the solutions are done in shell scripts. To run these, the filename
needs to be passed to the script instead of passing the input via stdin:

    ./xx.sh somefile
