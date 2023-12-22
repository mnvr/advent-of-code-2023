// The Garden of Forking Paths
//
// Part 1: Do a BFS to find all reachable nodes within 64 steps.

func readInput() -> [[Character]] {
    var result: [[Character]] = []
    while let line = readLine() {
        result.append(Array(line))
    }
    return result
}

let map = readInput()
print(map)
