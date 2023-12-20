func readInput() -> [String] {
    var result: [String] = []
    while let line = readLine() {
        result.append(line)
    }
    return result
}

let input = readInput()
print(input)
