/// An exploration of some basic graph traversals using Day 17's example data.

func readInput() -> [[Int]] {
    var result: [[Int]] = []
    while let line = readLine() {
        result.append(line.map { $0.wholeNumberValue! })
    }
    return result
}

struct Grid<T> {
    /// Swift doesn't generate Hashable conformance for tuples, so what could've
    /// been an (Int, Int) is now this.
    struct Index: Hashable {
        let x: Int
        let y: Int

        static func + (u: Index, v: Index) -> Index {
            Index(x: u.x + v.x, y: u.y + v.y)
        }
    }

    let items: [[T]]
    let maxIndex: Index

    init(items: [[T]]) {
        self.items = items
        self.maxIndex = Index(x: items[0].count - 1, y: items.count - 1)
    }

    private func inBounds(u: Index) -> Bool {
        u.x >= 0 && u.y >= 0 && u.x <= maxIndex.x && u.y <= maxIndex.y
    }

    func at(_ u: Index) -> T {
        items[u.y][u.x]
    }

    func adjacent(_ u: Index) -> [Index] {
        [ u + Index(x: +1, y: 0),
          u + Index(x: -1, y: 0),
          u + Index(x: 0, y: -1),
          u + Index(x: 0, y: +1),
        ].filter(inBounds)
    }
}

typealias Visitor<T> = (Grid<T>.Index, T) -> Void

func makePrintVisitor<T>(_ label: String) -> Visitor<T> {
    return { u, item in
        print("\(label) visiting item \(item) at index \(u)")
    }
}

/// This DFS, and BFS below, are Swift ports of
/// https://mrmr.io/random-mutations.

func dfs<T>(grid: Grid<T>, start: Grid<T>.Index, visit: Visitor<T>) {
    var pending = [start]
    var visited = Set<Grid<T>.Index>()
    while let u = pending.popLast() {
        if !visited.insert(u).inserted { continue }
        visit(u, grid.at(u))
        for v in grid.adjacent(u) {
            pending.append(v)
        }
    }
}

func bfs<T>(grid: Grid<T>, start: Grid<T>.Index, visit: Visitor<T>) {
    var pending = [start]
    var visited = Set<Grid<T>.Index>()
    // Unlike popLast which is O(1), removeFirst is O(n), which makes traversal
    // inefficient. For real programs, consider using a data structure that
    // provides a "popFirst", e.g. the Dequeue in the Swift Collections package.
    while !pending.isEmpty {
        let u = pending.removeFirst()
        if !visited.insert(u).inserted { continue }
        visit(u, grid.at(u))
        for v in grid.adjacent(u) {
            pending.append(v)
        }
    }
}

let input = readInput()
let grid = Grid(items: input)
print(grid)
dfs(grid: grid, start: .init(x: 0, y: 0), visit: makePrintVisitor("dfs"))
bfs(grid: grid, start: .init(x: 0, y: 0), visit: makePrintVisitor("bfs"))
