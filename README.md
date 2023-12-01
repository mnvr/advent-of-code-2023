Haskell solutions to [Advent of Code 2023](https://adventofcode.com/2023).

## Running

Install [Haskell using GHCup](https://www.haskell.org/ghcup/). Then, to run the
latest solution on the latest of its example inputs, use

    make

Other commands:

-   `make read` – interactively run the latest solution with input from stdin.
-   `make run` – run the latest solution on the actual input.

All of these are wrappers around the basic pattern of

    cat somefile | runghc 0x.hs
