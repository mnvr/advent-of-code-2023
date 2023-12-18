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
        /// Corresponds to a 90-deg counterclockwise rotation on our grid's
        /// coordinate system.
        func rotatedLeft() -> Index {
            Index(x: y, y: -x)
        }

        /// Multiplication by -i (the complex number, -√-1).
        ///
        /// Corresponds to a 90-deg clockwise rotation.
        func rotatedRight() -> Index {
            Index(x: -y, y: x)
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

    func adjacent(
        _ u: Index, heading: Index, steps: Int
    ) -> [(Index, Index, Int)] {
        adjacentCandidates(u, heading: heading, steps: steps)
            .filter { inBounds(u: $0.0) }
    }

    func adjacentCandidates(
        _ u: Index, heading h: Index, steps s: Int
    ) -> [(Index, Index, Int)] {
        let hl = h.rotatedLeft()
        let hr = h.rotatedRight()
        if s < 2 {
            return [(u + h, h, s + 1), (u + hl, hl, 0), (u + hr, hr, 0)]
        } else {
            return [(u + hl, hl, 0), (u + hr, hr, 0)]
        }
    }

    func edgeWeight(from u: Index, to v: Index) -> Int {
        at(v)
    }

    func showDistances(
        distances: [Index: Int], parents: [Index: Index],
        selectedPath: Set<Index>
    ) -> String {
        let tHighlight = "\u{001B}[0;0m"
        let tDim = "\u{001B}[2;80m"
        let tReset = "\u{001B}[0m"

        func parent(_ u: Index) -> String? {
            guard let p = parents[u] else { return nil }
            switch (p.x - u.x, p.y - u.y) {
                case (-1, 0): return "→"
                case (1, 0): return "←"
                case (0, -1): return "↓"
                case (0, 1): return "↑"
                default: return "⥀"
            }
        }

        var result = [String]()
        for (y, row) in items.enumerated() {
            for (x, item) in row.enumerated() {
                let u = Index(x: x, y: y)
                result.append(selectedPath.contains(u) ? tHighlight : tDim);
                if let d = distances[u], let p = parent(u) {
                    result.append("\(p) \(item) \(d)\t")
                } else {
                    result.append("  \(item)\t")
                }
                result.append(tReset);
            }
            result.append("\n")
        }
        return result.joined()
    }
}

typealias Visitor = (Grid.Index, Grid.Index, Int, Int) -> Void

func makePrintVisitor(_ label: String) -> Visitor {
    return { u, heading, steps, item in
        print("\(label) visiting item \(item) at index \(u) heading \(heading) steps \(steps)")
    }
}

/// Find the shortest path between `start` and `end` using Dijkstra's algorithm.
///
/// If end is not reachable from start, return nil.
func shortestPath(
    grid: Grid, start: Grid.Index, startHeadings: [Grid.Index],
    end: Grid.Index, visit: Visitor?
) -> Int? {
    var pending = startHeadings.map { (start, $0, 0) }
    var visited = Set<Grid.Index>()
    var distance = [start: 0]
    var parent = [start: start]
    var iterations = 0

    // The real algorithm requires a data structure that allows us to quickly
    // find the element with the least associated value, and pop it efficiently.
    // Here we do an (inefficient) simulation using only the standard library
    // data structures. For real programs, consider using a priority queue, like
    // the Heap in the Swift Collections package.
    func popNearest() -> [(Grid.Index, Grid.Index, Int)]? {
        // var u: Grid.Index?
        var ud = Int.max
        var ui: Int?
        for (vi, (v, _, _)) in pending.enumerated() {
            if let vd = distance[v], vd < ud {
                // u = v
                ud = vd
                ui = vi
            }
        }
        if let ui { return [pending.remove(at: ui)] }
        print("nothing to return from pending \(pending)")
        return nil
        // guard let u else { return nil }
        // let result = pending.filter { $0.0 == u }
        // pending.removeAll { $0.0 == u }
        // return result
    }

    func show() {
        var selectedPath = Set([start, end])
        var u = end
        while let v = parent[u], v != start {
            selectedPath.insert(v)
            u = v
        }

        print("after \(iterations) iterations")
        let vis = grid.showDistances(
            distances: distance, parents: parent, selectedPath: selectedPath)
        print(vis, terminator: "")
    }

    defer { show() }

    while let us = popNearest() {
        for (u, uh, usteps) in us {
            // if (u == end) { return distance[end] }

            iterations += 1
            if (iterations % 100 == 0) {
                show()
            }

            if !visited.insert(u).inserted { continue }
            let du = distance[u]!
            // visit?(u, uh, usteps, grid.at(u))
            for (v, vh, vsteps) in grid.adjacent(u, heading: uh, steps: usteps) {
                if visited.contains(v) { continue }
                let dv = distance[v] ?? Int.max
                let w = grid.edgeWeight(from: u, to: v)
                if dv > du + w {
                    distance[v] = du + w
                    parent[v] = u
                }
                pending.append((v, vh, vsteps))
            }
        }
    }

    return distance[end]
}

let input = readInput()
let grid = Grid(items: input)
// print(grid)
let sp = shortestPath(
    grid: grid,
    start: .init(x: 0, y: 0),
    startHeadings: [.init(x: 1, y: 0), .init(x: 0, y: 1)],
    end: grid.maxIndex,
    visit: makePrintVisitor("shortest-path")
)
print("shortest-path-result", sp ?? -1)
// print(grid.adjacentCandidates(.init(x: 1, y: 1), heading: .init(x: 1, y: 0)))
