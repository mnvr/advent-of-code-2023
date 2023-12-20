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
/// vertices (**Shoelace formula**).
func area(steps: [Step]) -> Int {
    var (px, py) = (0, 0)
    var s = 0
    for step in steps {
        var x, y : Int
        switch step.direction {
        case "R": (x, y) = (px + step.count, py)
        case "L": (x, y) = (px - step.count, py)
        case "U": (x, y) = (px, py - step.count)
        case "D": (x, y) = (px, py + step.count)
        default: fatalError()
        }
        s += (py + y) * (px - x)
        px = x
        py = y
    }
    return abs(s) / 2
}


let steps = readInput()
let ar = area(steps: steps)
print(ar)
