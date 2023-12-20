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
        default: fatalError()
        }
        count = Int(String(color[2...6]), radix:16)!
        s2.append(Step(direction: direction, count: count))
    }
    return (s1, s2)
}

/// Explanation: Shoelace formula + Circumference + 1
///
/// The interior area of a polygon with integral cartesian coordinates can be
/// computed by summing up the signed areas of the trapezoids formed by
/// consecutive pairs of vertices (**Shoelace formula**).
///
/// To get the total area (i.e. the interior area plus the edge cells), we can
/// just add the circumference as we circumnavigate it.
///
/// Finally, since the circumference didn't include the starting square, we add
/// 1 to account for it.
func area(steps: [Step]) -> Int {
    var px = 0, py = 0, s = 0
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
        s += step.count
        (px, py) = (x, y)
    }

    return abs(s) / 2 + 1
}


let (s1, s2) = readInput()
let a1 = area(steps: s1)
let a2 = area(steps: s2)
print(a1, a2)
