struct Step {
    let direction: Character
    let count: Int
}

func readInput() -> ([Step], [Step]) {
    var s1: [Step] = []
    var s2: [Step] = []
    while let line = readLine() {
        let splits = line.split(separator: " ")

        var direction = splits[0].first!
        var count = Int(splits[1])!
        s1.append(Step(direction: direction, count: count))

        let color = Array(splits[2])
        direction = switch(color[7]) {
        case "0": "R"
        case "1": "D"
        case "2": "L"
        case "3": "U"
        default: fatalError("\(color)")
        }
        count = Int(String(color[2...6]), radix:16)!
        s2.append(Step(direction: direction, count: count))
    }
    return (s1, s2)
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


let (s1, s2) = readInput()
let a1 = area(steps: s1)
let a2 = area(steps: s2)
print(a1, a2)
