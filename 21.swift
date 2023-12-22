// The Garden of Forking Paths
//
// Part 1: Do a BFS to find all reachable nodes within 64 steps.

typealias Map = [[Character]]

struct Coordinate: Hashable {
    let x, y: Int
}

struct Grid {
    let map: Map
    let start: Coordinate
    let max: Coordinate

    init(map: Map) {
        self.map = map
        self.start = Grid.findStart(map)
        self.max = Coordinate(x: map[0].count, y: map.count)
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

func bfs(_ map: Map) -> Int {
    var visited: Set<Coordinate> = Set()
    return 0
}

let grid = readInput()
print(grid)
