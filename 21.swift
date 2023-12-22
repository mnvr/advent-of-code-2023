// The Garden of Forking Paths

typealias Map = [[Character]]

struct Coordinate: Hashable {
    let x, y: Int
}

struct Grid {
    var map: Map
    let start: Coordinate
    let max: Coordinate

    init(map: Map) {
        self.map = map
        self.start = Grid.findStart(map)
        self.max = Coordinate(x: map[0].count - 1, y: map.count - 1)
    }

    static func findStart(_ map: Map) -> Coordinate {
        for (y, row) in map.enumerated() {
            for (x, c) in row.enumerated() {
                if c == "S" { return Coordinate(x: x, y: y) }
            }
        }
        fatalError()
    }

    func at(_ c: Coordinate) -> Character {
        map[c.y][c.x]
    }

    func neighbours(of c: Coordinate) -> [Coordinate] {
        potentialNeighbours(c).filter { inBounds($0) && at($0) != "#" }
    }

    func inBounds(_ c: Coordinate) -> Bool {
        c.x >= 0 && c.x <= max.x &&
        c.y >= 0 && c.y <= max.y
    }

    func potentialNeighbours(_ c: Coordinate) -> [Coordinate] {
        [(c.x - 1, c.y), (c.x + 1, c.y), (c.x, c.y - 1), (c.x, c.y + 1)]
            .map { Coordinate(x: $0, y: $1) }
    }

    func coordinates() -> [Coordinate] {
        var result: [Coordinate] = []
        for y in 0...max.y {
            for x in 0...max.x {
                result.append(Coordinate(x: x, y: y))
            }
        }
        return result
    }

    mutating func move() {
        func isOccupied(_ c: Character) -> Bool {
            c == "O" || c == "S"
        }

        let prevMap = map
        let cs = coordinates()
        for c in cs {
            if isOccupied(map[c.y][c.x]) {
                map[c.y][c.x] = "."
            }
        }
        for c in cs {
            if isOccupied(prevMap[c.y][c.x]) {
                for n in neighbours(of: c) {
                    map[n.y][n.x] = "O"
                }
            }
        }
    }

    var reachable: Int {
        map.reduce(0, { $1.reduce($0, { $0 + ($1 == "O" ? 1 : 0) })})
    }
}

extension Grid: CustomStringConvertible {
    var description: String {
        map.map { String($0) }.joined(separator: "\n")
    }
}

func readInput() -> Grid {
    var map: Map = []
    while let line = readLine() {
        map.append(Array(line))
    }
    return Grid(map: map)
}

func bfs(grid: Grid, maxStep: Int) -> Int {
    var pending = [(grid.start, 0)]
    var pi = 0

    var lastCells: Set<Coordinate> = Set()

    while pi < pending.count {
        let (c, step) = pending[pi]
        pi += 1

        if step == maxStep {
            lastCells.insert(c)
            continue
        }

        for n in grid.neighbours(of: c) {
            pending.append((n, step + 1))
        }
    }

    return lastCells.count
}

func move(grid: inout Grid, steps: Int) {
    for s in 1...steps {
        grid.move()
        if verbose > 0 {
            print("after step \(s) we reached \(grid.reachable) cells")
            print(grid)
            print("")
        }
    }
}

let verbose = switch CommandLine.arguments.last {
    case "-v": 1
    case "-vv": 2
    default: 0
}

var grid = readInput()
move(grid: &grid, steps: 6)
let p1 = grid.reachable
print(p1)

// let c = bfs(grid: grid, maxStep: 16)

// print(c)
