func readInput() -> [[Int]] {
    var result: [[Int]] = []
    while let line = readLine() {
        result.append(line.map { $0.wholeNumberValue! })
    }
    return result
}

/// A Gaussian integer, i.e. a complex number with integer coordinates. We only
/// implement the operations we need -- (+), (k *), (* i) and (* (-i)).
struct ComplexInt: Hashable {
    let x: Int
    let y: Int

    static func + (u: ComplexInt, v: ComplexInt) -> ComplexInt {
        ComplexInt(x: u.x + v.x, y: u.y + v.y)
    }

    static func * (k: Int, u: ComplexInt) -> ComplexInt {
        ComplexInt(x: k * u.x, y: k * u.y)
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
let maxDirection = directions.count - 1
/// We must move a minimum of 4 steps in a direction before we can turn. i.e. we
/// can turn at the 5th step onwards. Since we start counting from 0, this is 4.
var minStep = 4 // 0 for part 1
/// We can move a maximum of 10 steps in a direction before we can turn. We
/// start counting from minStep, which is zero-based, 10-1 == 9.
var maxStep = 9 // 2 for part 1

struct ExpandedItem {
    /// The original item / value
    let item: Int
    /// What direction are we facing
    let heading: ComplexInt
    /// How many steps have we taken in this direction.
    ///
    /// When this is less than minStep, we cannot move to this direction. When
    /// this is maxStep, we cannot move anymore in this direction.
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
                for step in minStep...maxStep {
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

struct Grid {
    struct Index: Hashable {
        let xy: ComplexInt
        let heading: ComplexInt
        let step: Int
    }

    let items: [[Int]]
    let expandedItems: [[[[ExpandedItem]]]]
    let maxIndex: Index

    init(items: [[Int]], expandedItems: [[[[ExpandedItem]]]]) {
        self.items = items
        self.expandedItems = expandedItems
        self.maxIndex = Index(
            xy: ComplexInt(x: items[0].count - 1, y: items.count - 1),
            heading: directions[maxDirection], step: maxStep)
    }

    var totalItems: Int {
        (maxIndex.xy.x + 1) * (maxIndex.xy.y + 1) *
        (maxDirection + 1) * ((maxStep - minStep) + 1)
    }

    private func inBounds(u: Index) -> Bool {
        u.xy.x >= 0 && u.xy.x <= maxIndex.xy.x &&
        u.xy.y >= 0 && u.xy.y <= maxIndex.xy.y &&
        u.step >= minStep && u.step <= maxStep
    }

    func at(_ u: Index) -> ExpandedItem {
        let hi = directions.firstIndex(of: u.heading)!
        return expanded[u.xy.y][u.xy.x][hi][u.step]
    }

    func at(xy: ComplexInt) -> Int {
        items[xy.y][xy.x]
    }

    func adjacent(_ u: Index) -> [Index] {
        adjacentCandidates(u).filter(inBounds)
    }

    func adjacentCandidates(_ u: Index) -> [Index] {
        let h = u.heading
        let hl = h.rotatedLeft()
        let hr = h.rotatedRight()

        let start = max(u.step + 1, minStep)
        let end = maxStep
        let inSameDirection: [Grid.Index] =
            if start <= end {
                 (start...end).map {
                    Index(xy: u.xy + h, heading: h, step: $0)
                }
            } else { [] }

        let afterTurning = minStep == 0 ? ([
            Index(xy: u.xy + hl, heading: hl, step: minStep),
            Index(xy: u.xy + hr, heading: hr, step: minStep),
        ]) : ([
            Index(xy: u.xy + minStep * hl, heading: hl, step: minStep),
            Index(xy: u.xy + minStep * hr, heading: hr, step: minStep),
        ])

        return (inSameDirection + afterTurning)
    }

    /// Precondition: v must be adjacent to u
    func edgeWeight( from u: Index, to v: Index) -> Int {
        // It is guarantee that at least u and v will share one of the cartesian
        // coordinates for their positions. Start at u, but move in the heading
        // of v. The edge weight is then the sum of the weights of all the nodes
        // we encounter along the way, including v (but not including u).
        var t = u.xy
        var w = 0
        repeat {
            t = t + v.heading
            w += at(xy: t)
        } while t != v.xy
        return w
    }

    /// Expand an (x, y) index into the original items array to indices
    /// corresponding to that item in the expanded items.
    func expandedIndex(xy: ComplexInt) -> [Index] {
        var result = [Index]()
        for direction in directions {
            for step in minStep...maxStep {
                result.append(
                    Index(xy: xy, heading: direction, step: step)
                )
            }
        }
        return result
    }
}

struct DijkstraState {
    let grid: Grid
    let iteration: Int
    let distance: [Grid.Index: Int]
    let parent: [Grid.Index: Grid.Index]
}

typealias Visitor = (DijkstraState) -> Void

/// Find the shortest path from `start` to all nodes using Dijkstra's algorithm.
func shortestPath(grid: Grid, start: Grid.Index, visit: Visitor? = nil
) -> DijkstraState {
    var pending = Set([start])
    var visited = Set<Grid.Index>()
    var distance = [start: 0]
    var parent: [Grid.Index: Grid.Index] = [:]
    var iteration = 0

    func state() -> DijkstraState {
        .init(grid: grid, iteration: iteration, distance: distance, parent: parent)
    }

    // The real algorithm requires a data structure that allows us to quickly
    // find the element with the least associated value, and pop it efficiently.
    // Here we do an (inefficient) simulation using only the standard library
    // data structures. For real programs, consider using a priority queue, like
    // the Heap in the Swift Collections package.
    func popNearest() -> Grid.Index? {
        var u: Grid.Index?
        var ud = Int.max
        for v in pending {
            if let vd = distance[v], vd < ud {
                u = v
                ud = vd
            }
        }
        if let u { pending.remove(u) }
        return u
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
            pending.insert(v)
        }
    }

    return state()
}

func trace(state: DijkstraState) {
    if state.iteration % 500 == 0 {
        let total = grid.totalItems
        print("iteration \(state.iteration) found tentative distances to \(state.distance.count) / \(total) items")
    }
}

extension Grid {
    /// Create string representation of the grid suitable for printing on a
    /// terminal.
    func renderToString(
        state: DijkstraState, start: Grid.Index, end: Grid.Index
    ) -> String {
        let tHighlight = "\u{001B}[0;0m"
        let tDim = "\u{001B}[2;80m"
        let tReset = "\u{001B}[0m"

        let grid = state.grid
        let distance = state.distance

        // Trace the path back from the end to the start.
        var selectedPath = Set([end])
        var u = end
        while let v = state.parent[u] {
            selectedPath.insert(v)
            u = v
        }

        func pathInfo(xy: ComplexInt) -> (distance: Int, parentDirection: String, steps: Int)? {
            for u in grid.expandedIndex(xy: xy) {
                if selectedPath.contains(u) {
                    if let parent = parentDirection(u), let d = distance[u] {
                        return (distance: d, parentDirection: parent, steps: u.step)
                    }
                }
            }
            return nil
        }

        func parentDirection(_ u: Index) -> String? {
            if u == end { return "·"}
            guard let p = state.parent[u] else { return "·" }
            switch(p.xy.x - u.xy.x, p.xy.y - u.xy.y) {
                case (let x, 0) where x < 0: return "→"
                case (let x, 0) where x > 0: return "←"
                case (0, let y) where y < 0: return "↓"
                case (0, let y) where y > 0: return "↑"
                default: fatalError()
            }
        }

        var result = [String]()
        let maxXY = grid.maxIndex.xy
        for y in 0...maxXY.y {
            for x in 0...maxXY.x {
                let xy = ComplexInt(x: x, y: y)
                let item = grid.at(xy: xy)
                if let (distance, parentDirection, step) = pathInfo(xy: xy) {
                    result.append(tHighlight);
                    result.append("\(parentDirection) \(item) \(distance) \(step) ")
                } else {
                    result.append(tDim)
                    result.append("  \(item) \t")
                }
                result.append(tReset)
            }
            result.append("\n")
        }
        return result.joined()
    }
}

func ourShortestPath(grid: Grid) -> Int? {
    func sp(heading: ComplexInt) -> Int? {
        let startXY = ComplexInt(x: 0, y: 0)
        let endXY = grid.maxIndex.xy;

        let start = Grid.Index(xy: startXY, heading: heading, step: 0)

        let state = shortestPath(grid: grid, start: start, visit: trace)

        // Find the minimum from amongst the distances of the original item.
        let endIndices = grid.expandedIndex(xy: endXY)
        var end: Grid.Index?
        var endDistance: Int?
        for u in endIndices {
            if let d = state.distance[u], d < (endDistance ?? Int.max) {
                end = u
                endDistance = d
            }
        }
        _ = end
        // if let end {
        //     print(
        //         grid.renderToString(state: state, start: start, end: end),
        //         terminator: "")
        // }
        return endDistance
    }

    // We need to head both ways. Whilst the same indexes (1, 0) and (0, 1) will
    // be the neighbours irrespective of which direction that we start in, the
    // step count will be different. The step count will be 1 for the direction
    // we're heading in, and 0 for the perpendicular one. So to cover both
    // combinations, we'll need to head both ways.

    return [
        sp(heading: .east),
        // When debugging, comment this.
        sp(heading: .south),
    ].compactMap({$0}).min()
}

let input = readInput()
let expanded = expand(items: input)
let grid = Grid(items: input, expandedItems: expanded)
let sp = ourShortestPath(grid: grid)
print("shortest-path-result", sp ?? -1)
