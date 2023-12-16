var grid: Grid = []
while let line = readLine() {
    grid.append(line)
}
print(energized(start: Beam(ix: [0, 0], d: .r)))

typealias Grid = [String]
// This is an (Int, Int), but Swift doesn't synthesize Hashable for tuples.
typealias Ix = [Int]
enum Direction { case l, r, u, d }

struct Beam: Hashable {
    let ix: Ix
    let d: Direction

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

func energized(start: Beam) -> Int {
    var visited = Set<Beam>()
    trace(beam: start, visited: &visited);
    return Set(visited.map({$0.ix})).count
}

func item(at beam: Beam) -> Character {
    let line = grid[beam.ix[1]]
    return line[line.index(line.startIndex, offsetBy: beam.ix[0])]
}

func trace(beam: Beam, visited: inout Set<Beam>) {
    if visited.contains(beam) {
        return
    }

    let next: [Beam]

    switch item(at: beam) {
        case "|" where isHorizontal(beam): next = beam.vsplit
        case "-" where isVertical(beam):   next = beam.hsplit
        case "/":
            switch beam.d {
                case .l: next = [beam.reflectD]
                case .r: next = [beam.reflectU]
                case .u: next = [beam.reflectR]
                case .d: next = [beam.reflectL]
            }
        case "\\":
            switch beam.d {
                case .l: next = [beam.reflectU]
                case .r: next = [beam.reflectD]
                case .u: next = [beam.reflectL]
                case .d: next = [beam.reflectR]
            }
        default: next = [beam.step]
    }

    visited.insert(beam)

    for n in next {
        if isInBounds(n) {
            trace(beam: n, visited: &visited)
        }
    }
}

func isHorizontal(_ beam: Beam) -> Bool {
    beam.d == .l || beam.d == .r
}

func isVertical(_ beam: Beam) -> Bool {
    !isHorizontal(beam)
}

func isInBounds(_ beam: Beam) -> Bool {
    let x = beam.ix[0]
    let y = beam.ix[1]
    return x >= 0 && x < grid[0].count && y >= 0 && y < grid.count
}
