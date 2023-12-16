var grid: [[Item]] = []
while let line = readLine() {
    grid.append(items(line))
}

var f = energized(by: Beam(ix: [0, 0], d: .r))
var m = edges().map({ energized(by: $0) }).max() ?? 0
print(f, m)

enum Item: Character {
    case dot = ".", hbar = "-", vbar = "|", fslash = "/", bslash = "\\"
}

func items(_ s: String) -> [Item] {  s.compactMap { Item(rawValue: $0) } }
func item(at beam: Beam) -> Item { grid[beam.ix[1]][beam.ix[0]] }

enum Direction { case l, r, u, d }

struct Beam: Hashable {
    // This is an (Int, Int), but Swift doesn't synthesize Hashable for tuples.
    let ix: [Int]
    let d: Direction

    var isHorizontal: Bool {  d == .l || d == .r }
    var isVertical: Bool {  d == .u || d == .d }

    var step: Beam {
        switch d {
            case .l: Beam(ix: [ix[0] - 1, ix[1]], d: d)
            case .r: Beam(ix: [ix[0] + 1, ix[1]], d: d)
            case .u: Beam(ix: [ix[0], ix[1] - 1], d: d)
            case .d: Beam(ix: [ix[0], ix[1] + 1], d: d)
        }
    }

    var hsplit: [Beam] {
        [Beam(ix: [ix[0] - 1, ix[1]], d: .l),
         Beam(ix: [ix[0] + 1, ix[1]], d: .r)]
    }

    var vsplit: [Beam] {
        [Beam(ix: [ix[0], ix[1] - 1], d: .u),
         Beam(ix: [ix[0], ix[1] + 1], d: .d)]
    }

    var reflectL: Beam { Beam(ix: [ix[0] - 1, ix[1]], d: .l) }
    var reflectR: Beam { Beam(ix: [ix[0] + 1, ix[1]], d: .r) }
    var reflectU: Beam { Beam(ix: [ix[0], ix[1] - 1], d: .u) }
    var reflectD: Beam { Beam(ix: [ix[0], ix[1] + 1], d: .d) }
}

func energized(by beam: Beam) -> Int {
    var visited = Set<Beam>()
    trace(beam: beam, visited: &visited)
    return Set(visited.map({$0.ix})).count
}

func trace(beam: Beam, visited: inout Set<Beam>) {
    if visited.contains(beam) {
        return
    }

    var beam = beam
    var next: [Beam]?

    while isInBounds(beam) && next == nil {
        visited.insert(beam)

        switch item(at: beam) {
        case .vbar where beam.isHorizontal: next = beam.vsplit
        case .hbar where beam.isVertical:   next = beam.hsplit
        case .fslash:
            switch beam.d {
                case .l: next = [beam.reflectD]
                case .r: next = [beam.reflectU]
                case .u: next = [beam.reflectR]
                case .d: next = [beam.reflectL]
            }
        case .bslash:
            switch beam.d {
                case .l: next = [beam.reflectU]
                case .r: next = [beam.reflectD]
                case .u: next = [beam.reflectL]
                case .d: next = [beam.reflectR]
            }
        default:
            beam = beam.step
        }
    }

    next?.filter(isInBounds).forEach { trace(beam: $0, visited: &visited) }
}

func isInBounds(_ beam: Beam) -> Bool {
    let x = beam.ix[0]
    let y = beam.ix[1]
    return x >= 0 && x < grid[0].count && y >= 0 && y < grid.count
}

func edges() -> [Beam] {
    let ny = grid.count
    let nx = grid.first?.count ?? 0

    var result: [Beam] = []
    for y in 0..<ny {
        result.append(Beam(ix: [0, y], d: .r))
        result.append(Beam(ix: [nx - 1, y], d: .l))
    }
    for x in 0..<nx {
        result.append(Beam(ix: [x, 0], d: .d))
        result.append(Beam(ix: [x, ny - 1], d: .u))
    }
    return result
}
