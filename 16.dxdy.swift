var grid: [[Item]] = []
while let line = readLine() {
    grid.append(items(line))
}

var f = energized(by: Beam(x: 0, y: 0, dx: 1, dy: 0))
var m = edges().map({ energized(by: $0) }).max() ?? 0
print(f, m)

enum Item: Character {
    case dot = ".", hbar = "-", vbar = "|", fslash = "/", bslash = "\\"
}

func items(_ s: String) -> [Item] { s.compactMap { Item(rawValue: $0) } }
func item(at beam: Beam) -> Item { grid[beam.y][beam.x] }

struct Beam: Hashable {
    let x, y, dx, dy: Int

    var isHorizontal: Bool { dx != 0 }
    var isVertical: Bool { dy != 0 }

    var step: Beam { Beam(x: x + dx, y: y + dy, dx: dx, dy: dy) }

    var hsplit: (Beam, Beam) {
        (Beam(x: x - 1, y: y, dx: -1, dy: 0),
         Beam(x: x + 1, y: y, dx: +1, dy: 0))
    }

    var vsplit: (Beam, Beam) {
        (Beam(x: x, y: y - 1, dx: 0, dy: -1),
         Beam(x: x, y: y + 1, dx: 0, dy: +1))
    }

    var reflectL: Beam { Beam(x: x - 1, y: y, dx: -1, dy: 0) }
    var reflectR: Beam { Beam(x: x + 1, y: y, dx: +1, dy: 0) }
    var reflectU: Beam { Beam(x: x, y: y - 1, dx: 0, dy: -1) }
    var reflectD: Beam { Beam(x: x, y: y + 1, dx: 0, dy: +1) }
}

func energized(by beam: Beam) -> Int {
    var visited = Set<Beam>()
    var next = [beam]
    var bt = beam

    while var beam = next.popLast() {
        if visited.contains(beam) {
            continue
        }

        while isInBounds(beam) {
            visited.insert(beam)

            switch item(at: beam) {
            case .vbar where beam.isHorizontal:
                (beam, bt) = beam.vsplit
                next.append(bt)
            case .hbar where beam.isVertical:
                (beam, bt) = beam.hsplit
                next.append(bt)
            case .fslash:
                switch (beam.dx, beam.dy) {
                    case (-1, 0): beam = beam.reflectD
                    case (+1, 0): beam = beam.reflectU
                    case (0, -1): beam = beam.reflectR
                    default: beam = beam.reflectL
                }
            case .bslash:
                switch (beam.dx, beam.dy) {
                    case (-1, 0): beam = beam.reflectU
                    case (+1, 0): beam = beam.reflectD
                    case (0, -1): beam = beam.reflectL
                    default: beam = beam.reflectR
                }
            default:
                beam = beam.step
            }
        }
    }

    return Set(visited.map({ [$0.x, $0.y] })).count
}

func isInBounds(_ beam: Beam) -> Bool {
    let x = beam.x
    let y = beam.y
    return x >= 0 && x < grid[0].count && y >= 0 && y < grid.count
}

func edges() -> [Beam] {
    let ny = grid.count
    let nx = grid.first?.count ?? 0

    var result: [Beam] = []
    for y in 0..<ny {
        result.append(Beam(x: 0, y: y, dx: 1, dy: 0))
        result.append(Beam(x: nx - 1, y: y, dx: -1, dy: 0))
    }
    for x in 0..<nx {
        result.append(Beam(x: x, y: 0, dx: 0, dy: 1))
        result.append(Beam(x: x, y: ny - 1, dx: 0, dy: -1))
    }
    return result
}
