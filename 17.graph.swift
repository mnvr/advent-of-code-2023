/// An exploration of some basic graph traversals using Day 17's example data.

func readInput() -> [[Int]] {
    var result: [[Int]] = []
    while let line = readLine() {
        result.append(line.map { $0.wholeNumberValue! })
    }
    return result
}

let input = readInput()
print(input)
