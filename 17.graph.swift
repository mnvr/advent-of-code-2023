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
        adjacentCandidates(u).filter(inBounds)
    }

    func adjacentCandidates(_ u: Index) -> [Index] {
        [ u + Index(x: +1, y: 0),
          u + Index(x: -1, y: 0),
          u + Index(x: 0, y: -1),
          u + Index(x: 0, y: +1) ]
    }

    /// Precondition: v must be adjacent to u
    func edgeWeight( from u: Index, to v: Index) -> Int {
        if let d = at(u) as? Int {
            return d
        }
        return 0
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

typealias VisitorDijkstra<T> = (
    Int, [Grid<T>.Index: Int], [Grid<T>.Index: Grid<T>.Index]
) -> Void

func makePrintVisitorDijkstra<T>(_ label: String) -> VisitorDijkstra<T> {
    return { iteration, distances, parents in
        print("\(label) iteration \(iteration) found tentative distances to \(distances.count) items")
    }
}

/// Find the shortest path between `start` and `end` using Dijkstra's algorithm.
///
/// If end is not reachable from start, return nil.
func shortestPath<T>(
    grid: Grid<T>, start: Grid<T>.Index, end: Grid<T>.Index,
    visit: Visitor<T>?, visitD: VisitorDijkstra<T>?
) -> Int? {
    var pending = [start]
    var visited = Set<Grid<T>.Index>()
    var distance = [start: 0]
    var parent: [Grid<T>.Index: Grid<T>.Index] = [:]
    var iteration = 0

    // The real algorithm requires a data structure that allows us to quickly
    // find the element with the least associated value, and pop it efficiently.
    // Here we do an (inefficient) simulation using only the standard library
    // data structures. For real programs, consider using a priority queue, like
    // the Heap in the Swift Collections package.
    func popNearest() -> Grid<T>.Index? {
        var ui: Int?
        var ud = Int.max
        for (vi, v) in pending.enumerated() {
            if let vd = distance[v], vd < ud {
                ui = vi
                ud = vd
            }
        }
        if let ui { return pending.remove(at: ui) }
        return nil
    }

    while let u = popNearest(), u != end {
        if !visited.insert(u).inserted { continue }

        visitD?(iteration, distance, parent)
        iteration += 1
        visit?(u, grid.at(u))

        let du = distance[u]!
        for v in grid.adjacent(u) {
            if visited.contains(v) { continue }
            let dv = distance[v] ?? Int.max
            let w = grid.edgeWeight(from: u, to: v)
            if dv > du + w {
                distance[v] = du + w
            }
            pending.append(v)
        }
    }

    return distance[end]
}

let input = readInput()
let grid = Grid(items: input)
print(grid)
dfs(grid: grid, start: .init(x: 0, y: 0), visit: makePrintVisitor("dfs"))
bfs(grid: grid, start: .init(x: 0, y: 0), visit: makePrintVisitor("bfs"))
let sp = shortestPath(
    grid: grid,
    start: .init(x: 0, y: 0),
    end: grid.maxIndex,
    visit: makePrintVisitor("shortest-path-step"),
    visitD: makePrintVisitorDijkstra("shortest-path"))
print("shortest-path-result", sp ?? -1)
