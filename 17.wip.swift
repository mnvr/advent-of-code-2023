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

func shortestPath(grid: Grid) -> Int? {
    let startNode = Node(x: 0, y: 0)
    func isEnd(node: Node) -> Bool { node == grid.lastNode }

    var dist: [Node: Int] = [startNode: 0]
    var seen: Set<Node> = Set()
    //var invDist: [Int: [Node]] = []
    //var minDist = 0

    func popNearest() -> (Node, Int)? {
        var u: Node?
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
        if isEnd(node: u) { return du }
        for v in neighbours(grid: grid, node: u) {
            let d = grid.items[v]!
            let dv = dist[v] ?? .max
            if du + d < dv {
                dist[v] = du + d
            }
        }
    }

    return nil
}

func neighbours(grid: Grid, node: Node) -> [Node] {
    let (x, y) = (node.x, node.y)
    return [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
      .map { Node(x: $0.0, y: $0.1) }
      .filter { grid.items[$0] != nil }
}

let grid = readInput()
let sp = shortestPath(grid: grid)
print(sp ?? 0)
