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

/// The interior area of a polygon with integral cartesian coordinates can be
/// computed by summing up the signed areas of the trapezoids formed by
/// consecutive pairs of vertices (**Shoelace formula**).
///
/// To get the total area (i.e. the interior area plus the edge cells), we can
/// just add the circumference as we circumnavigate it.
func area(steps: [Step]) -> Int {
    var (px, py) = (0, 0)
    var s = 0
    var i = 0
    var b = 0
    for step in steps {
        // print(px, py, s)
        var x, y : Int
        switch step.direction {
        case "R": (x, y) = (px + step.count, py)
        case "L": (x, y) = (px - step.count, py)
        case "U": (x, y) = (px, py - step.count)
        case "D": (x, y) = (px, py + step.count)
        default: fatalError()
        }
        s += (py + y) * (px - x)
        i += (py + y) * (px - x)
        s += step.count
        b += step.count
        // print(step.count, s)
        px = x
        py = y
    }
    // tie it back to the origin
    s += (py + 0) * (px - 0)
    i += (py + 0) * (px - 0)
    // print(px, py, s)
    // print("end i b", i, b)

    return (i + b + 2) / 2

    // return mabs(s) / 2
}


let steps = readInput()
let ar = area(steps: steps)
print(ar)
