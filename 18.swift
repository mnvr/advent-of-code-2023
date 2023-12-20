struct Step {
    let direction: Character
    let count: Int
    let color: String
}

func readInput() -> [Step] {
    var result: [Step] = []
    while let line = readLine() {
        let splits = line.split(separator: " ")
        let direction = splits[0].first!
        let count = Int(splits[1])!
        let color = String(splits[2])
        result.append(Step(direction: direction, count: count, color: color))
    }
    return result
}

/// The area of a polygon with integral cartesian coordinates can be computed by
/// summing up the signed areas of the trapezoids formed by consecutive pairs of
/// vertices (Shoelace formula).
// func area(input: String) -> Int {
//     let (x, y) = (0, 0)
//     for item in input {

//     }
// }

let input = readInput()
print(input)
