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
    // Create two starting cells, one for each axis of movement. Then, when
    // considering the neighbours, we never go straight, we always turn.
    let startL = Cell(node: startNode, direction: .l, moves: 1)
    let startD = Cell(node: startNode, direction: .d, moves: 1)
    func isEnd(_ cell: Cell) -> Bool { cell.node == grid.lastNode }

    func adj(_ u: Cell) -> some Sequence<Neighbour> {
        neighbours(grid: grid, moveRange: moveRange, cell: u)
    }

    var dist: [Cell: Int] = [startL: 0, startD: 0]
    var seen: Set<Cell> = Set()
    // Use the inverse of the distance map to simulate a priority queue.
    var inverseDistance = [0: Set([startL, startD])]

    func popNearest() -> (Cell, Int)? {
        while let d = inverseDistance.keys.min() {
            for v in inverseDistance[d]! {
                if !seen.contains(v) { return (v, d)  }
            }
            inverseDistance[d] = nil
        }
        return nil
    }

    while let (u, du) = popNearest() {
        if !seen.insert(u).inserted { continue }
        if isEnd(u) { return du }
        for (v, d) in adj(u) {
            let d2 = du + d
            let dv = dist[v] ?? .max
            if d2 < dv {
                dist[v] = d2
                inverseDistance[d2, default: Set()].insert(v)
            }
        }
    }

    return 0
}

typealias Neighbour = (cell: Cell, d: Int)

func neighbours(
    grid: Grid, moveRange: ClosedRange<Int>, cell: Cell
) -> some Sequence<Neighbour> {
    func make(_ xy: (Int, Int), _ direction: Direction, _ moves: Int) -> Cell {
        Cell(node: Node(x: xy.0, y: xy.1), direction: direction, moves: moves)
    }

    let cellRange = 1...moveRange.upperBound

    func seq(_ makeCell: (Int) -> Cell) -> some Sequence<Neighbour> {
        var s = 0
        return cellRange.compactMap { m in
            let cell = makeCell(m)
            guard let d = grid.items[cell.node] else { return nil }
            s += d
            return moveRange.contains(cell.moves) ? (cell: cell, d: s) : nil
        }
    }

    let (x, y) = (cell.node.x, cell.node.y)
    switch cell.direction {
    case .l, .r:
        return [seq({ m in make((x, y - m), .u, m) }),
                seq({ m in make((x, y + m), .d, m) })].joined()
    default:
        return [seq({ m in make((x - m, y), .r, m) }),
                seq({ m in make((x + m, y), .l, m) })].joined()
    }
}

let grid = readInput()
let p1 = shortestPath(grid: grid, moveRange: 1...3)
let p2 = shortestPath(grid: grid, moveRange: 4...10)
print(p1, p2)
