/// Day 17 Part 1 only

func readInput() -> [[Int]] {
    var result: [[Int]] = []
    while let line = readLine() {
        result.append(line.map { $0.wholeNumberValue! })
    }
    return result
}

struct Grid {
    struct Index: Hashable {
        let x: Int
        let y: Int

        static func + (u: Index, v: Index) -> Index {
            Index(x: u.x + v.x, y: u.y + v.y)
        }

        /// Multiplication by i (the complex number, √-1).
        ///
        /// Corresponds to a 90-deg counterclockwise rotation.
        func rotatedLeft() -> Index {
            Index(x: -y, y: x)
        }

        /// Multiplication by -i (the complex number, -√-1).
        ///
        /// Corresponds to a 90-deg clockwise rotation.
        func rotatedRight() -> Index {
            Index(x: y, y: -x)
        }
    }

    let items: [[Int]]
    let maxIndex: Index

    init(items: [[Int]]) {
        self.items = items
        self.maxIndex = Index(x: items[0].count - 1, y: items.count - 1)
    }

    private func inBounds(u: Index) -> Bool {
        u.x >= 0 && u.y >= 0 && u.x <= maxIndex.x && u.y <= maxIndex.y
    }

    func at(_ u: Index) -> Int {
        items[u.y][u.x]
    }

    func adjacent(_ u: Index, heading: Index) -> [(Index, Index)] {
        adjacentCandidates(u, heading: heading).filter { inBounds(u: $0.0) }
    }

    func adjacentCandidates(_ u: Index, heading h: Index) -> [(Index, Index)] {
        let hl = h.rotatedLeft()
        let hr = h.rotatedRight()
        return [ (u + h, h),
                 (u + hl, hl),
                 (u + hr, hr) ]
    }

    func edgeWeight( from u: Index, to v: Index) -> Int {
        at(u)
    }
}

typealias Visitor = (Grid.Index, Grid.Index, Int) -> Void

func makePrintVisitor(_ label: String) -> Visitor {
    return { u, heading, item in
        print("\(label) visiting item \(item) at index \(u), heading \(heading)")
    }
}

/// Find the shortest path between `start` and `end` using Dijkstra's algorithm.
///
/// If end is not reachable from start, return nil.
func shortestPath(
    grid: Grid, start: Grid.Index, startHeading: Grid.Index,
    end: Grid.Index, visit: Visitor?
) -> Int? {
    var pending = [(start, startHeading)]
    var visited = Set<Grid.Index>()
    var distance = [start: 0]

    // The real algorithm requires a data structure that allows us to quickly
    // find the element with the least associated value, and pop it efficiently.
    // Here we do an (inefficient) simulation using only the standard library
    // data structures. For real programs, consider using a priority queue, like
    // the Heap in the Swift Collections package.
    func popNearest() -> (Grid.Index, Grid.Index)? {
        var ui: Int?
        var ud = Int.max
        for (vi, (v, vh)) in pending.enumerated() {
            if let vd = distance[v], vd < ud {
                ui = vi
                ud = vd
            }
        }
        if let ui { return pending.remove(at: ui) }
        return nil
    }

    while let (u, uh) = popNearest(), u != end {
        if !visited.insert(u).inserted { continue }
        let du = distance[u]!
        visit?(u, uh, grid.at(u))
        for (v, vh) in grid.adjacent(u, heading: uh) {
            if visited.contains(v) { continue }
            let dv = distance[v] ?? Int.max
            let w = grid.edgeWeight(from: u, to: v)
            if dv > du + w {
                distance[v] = du + w
            }
            pending.append((v, vh))
        }
    }

    return distance[end]
}

let input = readInput()
let grid = Grid(items: input)
print(grid)
let sp = shortestPath(
    grid: grid, start: .init(x: 0, y: 0), startHeading: .init(x: 1, y: 0),
    end: grid.maxIndex, visit: makePrintVisitor("shortest-path"))
print("shortest-path-result", sp ?? -1)
