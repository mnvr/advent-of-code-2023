/// An exploration of some basic graph traversals using Day 17's example data.

func readInput() -> [[Int]] {
    var result: [[Int]] = []
    while let line = readLine() {
        var numbers: [Int] = []
        for character in line {
            numbers.append(character.wholeNumberValue!)
        }
        result.append(numbers)
    }
    return result
}

let input = readInput()
print(input)
