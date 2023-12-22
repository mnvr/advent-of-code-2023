// The Garden of Forking Paths
//
// Part 1: Do a BFS to find all reachable nodes within 64 steps.

typealias Map = [[Character]]
typealias Coordinate = (x: Int, y: Int)

struct Grid {
    let map: Map
    let start: Coordinate
    let max: Coordinate

    init(map: Map) {
        self.map = map
        self.start = Grid.findStart(map)
        self.max = (map[0].count, map.count)
    }

    static func findStart(_ map: Map) -> Coordinate {
        for (y, row) in map.enumerated() {
            for (x, c) in row.enumerated() {
                if c == "S" { return (x, y) }
            }
        }
        fatalError()
    }

    func neighbours(of c: Coordinate) -> [Coordinate] {
        return []
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

// func bfs(_ map: Map) -> Int {
//     var visited: Set
// }

let grid = readInput()
print(grid)
