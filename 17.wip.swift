struct Grid {
    let items: [Node: Int]
    let lastNode: Node
}

struct Node: Hashable {
    let x, y : Int
}

func readInput() -> Grid {
    var items = [Node: Int]()
    var y = 0, x = 0
    while let line = readLine() {
        x = 0
        for c in line {
            items[Node(x: x, y: y)] = c.wholeNumberValue!
            x += 1
        }
        y += 1
    }
    return Grid(items: items, lastNode: .init(x: x - 1, y: y - 1))
}

enum Direction {
    case l, r, u, d
}

struct Cell: Hashable {
    let node: Node
    let direction: Direction
    let moves: Int
}

func shortestPath(grid: Grid, moveRange: ClosedRange<Int>) -> Int? {
    let startNode = Node(x: 0, y: 0)
    // Setting moves to 0 to allows us to equally consider both left and down
    // neighbours.
    let startCell = Cell(node: startNode, direction: .l, moves: 0)
    func isEnd(_ cell: Cell) -> Bool { cell.node == grid.lastNode }

    func adj(_ u: Cell) -> [Cell] {
        neighbours(grid: grid, moveRange: moveRange, cell: u)
    }

    var dist: [Cell: Int] = [startCell: 0]
    var seen: Set<Cell> = Set()
    //var invDist: [Int: [Node]] = []
    //var minDist = 0

    func popNearest() -> (Cell, Int)? {
        var u: Cell?
        var du: Int = .max
        for (v, dv) in dist {
            if dv < du && !seen.contains(v) {
                u = v
                du = dv
            }
        }
        if let u { return (u, du) }
        return nil
    }

    while let (u, du) = popNearest() {
        if !seen.insert(u).inserted { continue }
        if isEnd(u) { return du }
        for v in adj(u) {
            let d = grid.items[v.node]!
            let dv = dist[v] ?? .max
            if du + d < dv {
                dist[v] = du + d
            }
        }
    }

    return nil
}

func neighbours(grid: Grid, moveRange: ClosedRange<Int>, cell: Cell) -> [Cell] {
    func make(_ xy: (Int, Int), _ direction: Direction, _ moves: Int) -> Cell {
        Cell(node: Node(x: xy.0, y: xy.1), direction: direction, moves: moves)
    }

    let node = cell.node
    let moves = cell.moves
    let (x, y) = (node.x, node.y)
    let cells: [Cell]
    switch cell.direction {
    case .l:
        cells = [make((x + 1, y), .l, moves + 1),
                 make((x, y - 1), .u, 1),
                 make((x, y + 1), .d, 1)]
    case .r:
        cells = [make((x - 1, y), .r, moves + 1),
                 make((x, y - 1), .u, 1),
                 make((x, y + 1), .d, 1)]
    case .u:
        cells = [make((x, y - 1), .u, moves + 1),
                 make((x - 1, y), .r, 1),
                 make((x + 1, y), .l, 1)]
    case .d:
        cells = [make((x, y + 1), .d, moves + 1),
                 make((x - 1, y), .r, 1),
                 make((x + 1, y), .l, 1)]
    }
    return cells
      .filter { grid.items[$0.node] != nil }
      .filter { moveRange.contains($0.moves) }
}

let grid = readInput()
let p1 = shortestPath(grid: grid, moveRange: 1...3)
// let p1 = shortestPath(grid: grid, moveRange: 4...10)
print(p1 ?? 0)
