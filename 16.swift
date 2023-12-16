var grid: Grid = []
while let line = readLine() {
    grid.append(line)
}
let ny = grid.count
let nx = grid.first?.count ?? 0
var visited = Set<Beam>()
if (nx > 0) {
    trace(beam: Beam(ix: [0, 0], d: .r), visited: &visited)
}
print(tileCount(visited))

var m = 0
for y in 0..<ny {
    trace(beam: Beam(ix: [0, y], d: .r), visited: &visited)
    trace(beam: Beam(ix: [nx - 1, y], d: .l), visited: &visited)
}
for x in 0..<nx {
    trace(beam: Beam(ix: [x, 0], d: .d), visited: &visited)
    trace(beam: Beam(ix: [x, ny - 1], d: .u), visited: &visited)
}
print(tileCount(visited))

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

func tileCount(_ visited: Set<Beam>) -> Int {
    Set(visited.map({$0.ix})).count
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
