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

// MARK: P2 ---

// Each cell here tracks if it is occupied in the superposition of all the
// possible grids that are overlaid on top of each other. This is easy to do for
// this particular problem - we can just wrap around at the bounds. This works
// because the next step does not depend on the neighbours (like in the game of
// life), but instead we can just propogate our current count of occupancy to
// our neighbours.
struct QuantumGrid {
    var map: [[Int]]
    let start: Coordinate
    let max: Coordinate

    init(map: Map) {
        self.map = Self.makeCountMap(map)
        self.start = Self.findStart(map)
        self.max = Coordinate(x: map[0].count - 1, y: map.count - 1)
    }

    static func makeCountMap(_ map: Map) -> [[Int]] {
        var result = [[Int]]()
        for row in map {
            result.append(row.map { cell in switch cell {
                case "#": -1
                case "S": 1
                default: 0
            }})
        }
        return result
    }

    static func findStart(_ map: Map) -> Coordinate {
        for (y, row) in map.enumerated() {
            for (x, c) in row.enumerated() {
                if c == "S" { return Coordinate(x: x, y: y) }
            }
        }
        fatalError()
    }

    func at(_ c: Coordinate) -> Int {
        map[c.y][c.x]
    }

    func neighbours(of c: Coordinate) -> [Coordinate] {
        potentialNeighbours(c).filter { at($0) >= 0 }
    }

    func wrapAtBounds(_ c: Coordinate) -> Coordinate {
        Coordinate(x: c.x < 0 ? ((max.x + 1) + c.x) : (c.x % (max.x + 1)),
                   y: c.y < 0 ? ((max.y + 1) + c.y) : (c.y % (max.y + 1)))
    }

    func potentialNeighbours(_ c: Coordinate) -> [Coordinate] {
        [(c.x - 1, c.y), (c.x + 1, c.y), (c.x, c.y - 1), (c.x, c.y + 1)]
            .map { Coordinate(x: $0, y: $1) }
            .map { wrapAtBounds($0) }
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
        func isOccupied(_ i: Int) -> Bool {
            i > 0
        }

        let prevMap = map
        let cs = coordinates()
        for c in cs {
            if isOccupied(map[c.y][c.x]) {
                map[c.y][c.x] = 0
            }
        }
        for c in cs {
            let p = prevMap[c.y][c.x]
            if p > 0 {
                for n in neighbours(of: c) {
                    print("map[\(n.y)][\(n.x)] += \(p)")
                    map[n.y][n.x] += p
                }
            }
        }
    }

    var reachable: Int {
        map.reduce(0, { $1.reduce($0, { $0 + ($1 > 0 ? 1 : 0) })})
    }
}

extension QuantumGrid: CustomStringConvertible {
    var description: String {
        func mapCell(_ i: Int) -> String {
            String(i < 0 ? "#" : (i == 0 ? "." : "O"))
        }
        func mapRow(_ xs: [Int]) -> String {
            xs.map({ mapCell($0) }).joined(separator: "")
        }
        return map.map { mapRow($0) }.joined(separator: "\n")
    }
}

// MARK: Normal programming resumes

// For running under LLDB, where reading from stdin is a pain.
//
// However, we the input fixed, we can do this:
//
//     swiftc -O -o out/21.sw 21.swift
//     lldb out/21.sw
//     > run

let example = """
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
"""

func _readInput() -> (Grid, QuantumGrid) {
    var map: Map = []
    while let line = readLine() {
        map.append(Array(line))
    }
    return (Grid(map: map), QuantumGrid(map: map))
}

func readInput() -> (Grid, QuantumGrid) {
    var map: Map = []
    for line in example.split(separator: "\n") {
        map.append(Array(line))
    }
    return (Grid(map: map), QuantumGrid(map: map))
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

func quantumMove(grid: inout QuantumGrid, steps: Int) {
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

var (grid, quantumGrid) = readInput()
// move(grid: &grid, steps: 6)
// let p1 = grid.reachable
// print(p1)

quantumMove(grid: &quantumGrid, steps: 10)
let p2 = quantumGrid.reachable
print(p2)

// let c = bfs(grid: grid, maxStep: 16)

// print(c)
