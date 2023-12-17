var grid: [[Item]] = []
while let line = readLine() {
    grid.append(items(line))
}

var f = energized(by: Beam(x: 0, y: 0, d: .r))
var m = edges().map({ energized(by: $0) }).max() ?? 0
print(f, m)

enum Item: Character {
    case dot = ".", hbar = "-", vbar = "|", fslash = "/", bslash = "\\"
}

func items(_ s: String) -> [Item] { s.compactMap { Item(rawValue: $0) } }
func item(at beam: Beam) -> Item { grid[beam.y][beam.x] }

enum Direction { case l, r, u, d }

struct Beam: Hashable {
    let x, y: Int
    let d: Direction

    var isHorizontal: Bool {  d == .l || d == .r }
    var isVertical: Bool {  d == .u || d == .d }

    var step: Beam {
        switch d {
            case .l: Beam(x: x - 1, y: y, d: d)
            case .r: Beam(x: x + 1, y: y, d: d)
            case .u: Beam(x: x, y: y - 1, d: d)
            case .d: Beam(x: x, y: y + 1, d: d)
        }
    }

    var hsplit: (Beam, Beam) {
        (Beam(x: x - 1, y: y, d: .l),  Beam(x: x + 1, y: y, d: .r))
    }

    var vsplit: (Beam, Beam) {
        (Beam(x: x, y: y - 1, d: .u),  Beam(x: x, y: y + 1, d: .d))
    }

    var reflectL: Beam { Beam(x: x - 1, y: y, d: .l) }
    var reflectR: Beam { Beam(x: x + 1, y: y, d: .r) }
    var reflectU: Beam { Beam(x: x, y: y - 1, d: .u) }
    var reflectD: Beam { Beam(x: x, y: y + 1, d: .d) }
}

func energized(by beam: Beam) -> Int {
    var visited = Set<Beam>()
    trace(beam: beam, visited: &visited)
    return Set(visited.map({ [$0.x, $0.y] })).count
}

func trace(beam: Beam, visited: inout Set<Beam>) {
    if visited.contains(beam) {
        return
    }

    var (beam, bt) = (beam, beam)
    var next: [Beam] = []

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
            switch beam.d {
                case .l: beam = beam.reflectD
                case .r: beam = beam.reflectU
                case .u: beam = beam.reflectR
                case .d: beam = beam.reflectL
            }
        case .bslash:
            switch beam.d {
                case .l: beam = beam.reflectU
                case .r: beam = beam.reflectD
                case .u: beam = beam.reflectL
                case .d: beam = beam.reflectR
            }
        default:
            beam = beam.step
        }
    }

    next.filter(isInBounds).forEach { trace(beam: $0, visited: &visited) }
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
        result.append(Beam(x: 0, y: y, d: .r))
        result.append(Beam(x: nx - 1, y: y, d: .l))
    }
    for x in 0..<nx {
        result.append(Beam(x: x, y: 0, d: .d))
        result.append(Beam(x: x, y: ny - 1, d: .u))
    }
    return result
}
