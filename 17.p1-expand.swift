func readInput() -> [[Int]] {
    var result: [[Int]] = []
    while let line = readLine() {
        result.append(line.map { $0.wholeNumberValue! })
    }
    return result
}

/// A Gaussian integer, i.e. a complex number with integer coordinates. We only
/// implement the operations we need -- (+),  (* i) and (* (-i)).
struct ComplexInt: Hashable {
    let x: Int
    let y: Int

    static func + (u: ComplexInt, v: ComplexInt) -> ComplexInt {
        ComplexInt(x: u.x + v.x, y: u.y + v.y)
    }

    /// Multiplication by i (the complex number, √-1).
    ///
    /// Corresponds to a 90-deg counterclockwise rotation on our grid's
    /// coordinate system.
    func rotatedLeft() -> ComplexInt {
        ComplexInt(x: y, y: -x)
    }

    /// Multiplication by -i (the complex number, -√-1).
    ///
    /// Corresponds to a 90-deg clockwise rotation.
    func rotatedRight() -> ComplexInt {
        ComplexInt(x: -y, y: x)
    }

    static let
        north = ComplexInt(x: 0, y: -1),
        south = ComplexInt(x: 0, y: +1),
        east = ComplexInt(x: +1, y: 0),
        west = ComplexInt(x: -1, y: 0)
}

let directions: [ComplexInt] = [.north, .south, .east, .west]
let maxStep = 3

struct ExpandedItem {
    /// The original item / value
    let item: Int
    /// What direction are we facing
    let heading: ComplexInt
    /// How many steps have we taken in this direction. When this is 3, we
    /// cannot move anymore in this direction.
    let steps: Int
}

/// Expand each item into a 2D-array of 3-tuples (an `ExpandedItem`).
///
/// Each such 3-tuple encodes the original value, the direction we entered this
/// grid item from, and the step count when we entered.
func expand(items: [[Int]]) -> [[[[ExpandedItem]]]] {
    var expanded4 = [[[[ExpandedItem]]]]()
    for row in items {
        var expanded3 = [[[ExpandedItem]]]()
        for item in row {
            var expanded2 = [[ExpandedItem]]()
            for direction in directions {
                var expanded1 = [ExpandedItem]()
                for step in 0...maxStep {
                    let expanded = ExpandedItem(
                        item: item, heading: direction, steps: step
                    )
                    expanded1.append(expanded)
                }
                expanded2.append(expanded1)
            }
            expanded3.append(expanded2)
        }
        expanded4.append(expanded3)
    }
    return expanded4
}

struct Grid<T> {
    struct Index: Hashable {
        let xy: ComplexInt
        let heading: ComplexInt
        let step: Int
    }

    let items: [[[[T]]]]
    let maxIndex: Index

    init(items: [[[[T]]]]) {
        self.items = items
        self.maxIndex = Index(
            xy: ComplexInt(x: items[0].count - 1, y: items.count - 1),
            heading: directions[directions.count - 1], step: maxStep
        )
    }

    private func inBounds(u: Index) -> Bool {
        u.xy.x >= 0 && u.xy.x <= maxIndex.xy.x &&
        u.xy.y >= 0 && u.xy.y <= maxIndex.xy.y
        // u.heading >= 0 && u.heading <= maxIndex.heading &&
        // u.steps >= 0 && u.steps <= maxIndex.steps
    }

    func at(_ u: Index) -> T {
        let hi = directions.firstIndex(of: u.heading)!
        return items[u.xy.y][u.xy.x][hi][u.step]
    }

    func adjacent(_ u: Index) -> [Index] {
        adjacentCandidates(u).filter(inBounds)
    }

    func adjacentCandidates(_ u: Index) -> [Index] {
        let h = u.heading
        let hl = h.rotatedLeft()
        let hr = h.rotatedRight()

        return [
            u.step < 2 ? Index(xy: u.xy + h, heading: h, step: u.step + 1) : nil,
            Index(xy: u.xy + hl, heading: hl, step: 0),
            Index(xy: u.xy + hr, heading: hr, step: 0),
        ].compactMap { $0 }
    }

    /// Precondition: v must be adjacent to u
    func edgeWeight( from u: Index, to v: Index) -> Int {
        if let e = at(v) as? ExpandedItem {
            return e.item
        }
        return 0
    }

    /// Expand an (x, y) index into the original items array to indices
    /// corresponding to that item in the expanded items.
    func expandedIndex(xy: ComplexInt) -> [Index] {
        var result = [Index]()
        for direction in directions {
            for step in 0...maxStep {
                result.append(
                    Index(xy: xy, heading: direction, step: step)
                )
            }
        }
        return result
    }

    func expandMaxIndex() -> [Index] {
        expandedIndex(xy: maxIndex.xy)
    }
}

struct DijkstraState<T> {
    let iteration: Int
    let distance: [Grid<T>.Index: Int]
    let parent: [Grid<T>.Index: Grid<T>.Index]
}

typealias Visitor<T> = (DijkstraState<T>) -> Void

func makePrintVisitor<T>(_ label: String) -> Visitor<T> {
    return { state in
        print("\(label) iteration \(state.iteration) found tentative distances to \(state.distance.count) items")
    }
}

/// Find the shortest path from `start` to all nodes using Dijkstra's algorithm.
func shortestPath<T>(grid: Grid<T>, start: Grid<T>.Index, visit: Visitor<T>? = nil
) -> DijkstraState<T> {
    var pending = [start]
    var visited = Set<Grid<T>.Index>()
    var distance = [start: 0]
    var parent: [Grid<T>.Index: Grid<T>.Index] = [:]
    var iteration = 0

    func state() -> DijkstraState<T> {
        .init(iteration: iteration, distance: distance, parent: parent)
    }

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

    while let u = popNearest() {
        if !visited.insert(u).inserted { continue }

        visit?(state())
        iteration += 1

        let du = distance[u]!
        for v in grid.adjacent(u) {
            if visited.contains(v) { continue }
            let dv = distance[v] ?? Int.max
            let w = grid.edgeWeight(from: u, to: v)
            if dv > du + w {
                distance[v] = du + w
                parent[v] = u
            }
            pending.append(v)
        }
    }

    return state()
}

func ourShortestPath(grid: Grid<ExpandedItem>) -> Int? {
    func sp(heading: ComplexInt) -> Int? {
        let topLeft = ComplexInt(x: 0, y: 0)
        let spState = shortestPath(
            grid: grid, start: .init(xy: topLeft, heading: .east, step: 1))
        // Find the minimum from amongst the distances of the original item
        let endIndices = grid.expandMaxIndex()
        let endDistance = endIndices.compactMap { spState.distance[$0] }.min()
        return endDistance
    }

    return [sp(heading: .east), sp(heading: .south)].compactMap({$0}).min()
}

let input = readInput()
let expanded = expand(items: input)
let grid = Grid(items: expanded)

let sp = ourShortestPath(grid: grid)
print("shortest-path-result", sp ?? -1)
