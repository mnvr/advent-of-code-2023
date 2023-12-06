Haskell¹ solutions to [Advent of Code 2023](https://adventofcode.com/2023).

<small>¹ The primary solutions are is Haskell. But for some problems, I've also
done variations using other means and languages. </small>

## Running

Install [Haskell using GHCup](https://www.haskell.org/ghcup/). Then, to run the
latest solution on the latest of its example inputs, use

    make

Other commands:

-   `make read` – interactively run, reading input from stdin.
-   `make run` – run on the actual input.
-   `make test` – same as `make run`, but also print the latest expected answer.
-   `make watch` – same as `make`, but start a fswatch to automatically re-run
    it whenever the .hs file is changed.
-   `make verify` - Run `make test`, but for all days so far, in reverse.
-   `make o` - same as `make test`, but first compile the .hs file using GHC
    with optimizations enabled, and also print timings. To remove these, use
    `make clean`.

All of these are wrappers around the basic pattern of

    cat somefile | runghc xx.hs

Some of the solutions are done in shell scripts. To run these, the filename
needs to be passed to the script instead of passing the input via stdin (this is
to keep the scripts simple – reading stdin in a script isn't a snap):

    ./xx.sh somefile
