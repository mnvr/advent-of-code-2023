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

func shortestPath(grid: Grid, moveRange: ClosedRange<Int>) -> Int {
    let startNode = Node(x: 0, y: 0)
    // Setting moves to 0 to allows us to equally consider both left and down
    // neighbours.
    let startCell = Cell(node: startNode, direction: .l, moves: 0)
    func isEnd(_ cell: Cell) -> Bool { cell.node == grid.lastNode }

    func adj(_ u: Cell) -> [Neighbour] {
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
        for n in adj(u) {
            let v = n.cell
            let d = n.d
            let dv = dist[v] ?? .max
            if du + d < dv {
                dist[v] = du + d
            }
        }
    }

    return 0
}

struct Neighbour {
    let cell: Cell
    let d: Int
}

func neighbours(grid: Grid, moveRange: ClosedRange<Int>, cell: Cell) -> [Neighbour] {
    let node = cell.node
    let moves = cell.moves
    let (x, y) = (node.x, node.y)
    let nbrs: [[Neighbour]]

    func make(_ xy: (Int, Int), _ direction: Direction, _ moves: Int) -> Cell {
        Cell(node: Node(x: xy.0, y: xy.1), direction: direction, moves: moves)
    }

    let cellRange = 1...moveRange.upperBound

    func seq(_ makeCell: (Int) -> Cell) -> [Neighbour] {
        var s = 0
        return Array(cellRange.compactMap { m in
            let cell = makeCell(m)
            guard let d = grid.items[cell.node] else { return nil }
            s += d
            if moveRange.contains(cell.moves) {
                return Neighbour(cell: cell, d: s)
            }
            return nil
        })
    }

    switch cell.direction {
    case .l:
        nbrs = [seq({ m in make((x + m, y), .l, moves + m) }),
                seq({ m in make((x, y - m), .u, m) }),
                seq({ m in make((x, y + m), .d, m) })]
    case .r:
        nbrs = [seq({ m in make((x - m, y), .r, moves + m) }),
                seq({ m in make((x, y - m), .u, m) }),
                seq({ m in make((x, y + m), .d, m) })]
    case .u:
        nbrs = [seq({ m in make((x, y - m), .u, moves + m) }),
                seq({ m in make((x - m, y), .r, m) }),
                seq({ m in make((x + m, y), .l, m) })]
    case .d:
        nbrs = [seq({ m in make((x, y + m), .d, moves + m) }),
                seq({ m in make((x - m, y), .r, m) }),
                seq({ m in make((x + m, y), .l, m) })]
    }
    return nbrs.flatMap { $0 }
}

let grid = readInput()
let p1 = shortestPath(grid: grid, moveRange: 1...3)
let p2 = shortestPath(grid: grid, moveRange: 4...10)
print(p1, p2)
