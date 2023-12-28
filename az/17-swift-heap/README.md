A variation of [17.swift](../../17.swift) that uses the (preview) Heap from the
[Swift Collections package](https://github.com/apple/swift-collections).

The other Swift code in the solutions doesn't rely on any packages, just uses
the standard library, but for this variation we specifically want to see how
much of an impact using a real heap (for storing the Dijkstra priority queue)
makes, so we do want to install the package. Since installing a package
necessitates having a `Package.swift`, the easiest way is to keep this solution
in a separate directory.

To build and run, just do

    cat ../../examples/17 | swift run

There is also a helper make command that builds an optimized binary, runs it
with the full input (must be present in your checkout!), and also prints the
time it took:

    make time
