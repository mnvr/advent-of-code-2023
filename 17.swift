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
    // Starting with 0 moves allows us to consider both left and down neighbours
    // equally.
    let startCell = Cell(node: Node(x: 0, y: 0), direction: .l, moves: 0)
    func isEnd(_ cell: Cell) -> Bool { cell.node == grid.lastNode }

    func adj(_ u: Cell) -> any Sequence<Neighbour> {
        neighbours(grid: grid, moveRange: moveRange, cell: u)
    }

    var dist: [Cell: Int] = [startCell: 0]
    var seen: Set<Cell> = Set()
    var invDist: [Int: [Cell]] = [0: [startCell]]
    var minDist = 0

    func popNearest() -> (Cell, Int)? {
        // Use an inverse distance map to simulate a priority queue.
        guard minDist < invDist.count else { return nil }
        if var unseen = invDist[minDist]?.filter({ !seen.contains($0) }),
            let v = unseen.popLast() {
            let d = minDist
            if unseen.count == 0 { minDist += 1 }
            return (v, d)
        } else {
            minDist += 1
            return popNearest()
        }
    }

    while let (u, du) = popNearest() {
        if !seen.insert(u).inserted { continue }
        if isEnd(u) { return du }
        for (v, d) in adj(u) {
            let d2 = du + d
            let dv = dist[v] ?? .max
            if d2 < dv {
                dist[v] = d2
                invDist[d2, default: []].append(v)
            }
        }
    }

    return 0
}

typealias Neighbour = (cell: Cell, d: Int)

func neighbours(
    grid: Grid, moveRange: ClosedRange<Int>, cell: Cell
) -> any Sequence<Neighbour> {
    let node = cell.node
    let moves = cell.moves
    let (x, y) = (node.x, node.y)

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
            return moveRange.contains(cell.moves) ? (cell: cell, d: s) : nil
        })
    }

    switch cell.direction {
    case .l:
        return [seq({ m in make((x + m, y), .l, moves + m) }),
                seq({ m in make((x, y - m), .u, m) }),
                seq({ m in make((x, y + m), .d, m) })].joined()
    case .r:
        return [seq({ m in make((x - m, y), .r, moves + m) }),
                seq({ m in make((x, y - m), .u, m) }),
                seq({ m in make((x, y + m), .d, m) })].joined()
    case .u:
        return [seq({ m in make((x, y - m), .u, moves + m) }),
                seq({ m in make((x - m, y), .r, m) }),
                seq({ m in make((x + m, y), .l, m) })].joined()
    case .d:
        return [seq({ m in make((x, y + m), .d, moves + m) }),
                seq({ m in make((x - m, y), .r, m) }),
                seq({ m in make((x + m, y), .l, m) })].joined()
    }
}

let grid = readInput()
let p1 = shortestPath(grid: grid, moveRange: 1...3)
let p2 = shortestPath(grid: grid, moveRange: 4...10)
print(p1, p2)
