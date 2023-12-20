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

struct ExpandedItem {
    /// The original item / value
    let item: Int
    /// What direction are we facing
    let heading: ComplexInt
    /// How many blocks have we already moved in this direction when getting to
    /// the current item.
    let moves: Int
}

/// Expand each item into a 2D-array of 3-tuples (an `ExpandedItem`).
///
/// Each such 3-tuple encodes the original value, the direction we entered this
/// grid item from, and the number of blocks we have moved in that direction.
func expand(items: [[Int]], validMoves: ClosedRange<Int>) -> [[[[ExpandedItem]]]] {
    var expanded4 = [[[[ExpandedItem]]]]()
    for row in items {
        var expanded3 = [[[ExpandedItem]]]()
        for item in row {
            var expanded2 = [[ExpandedItem]]()
            for direction in directions {
                var expanded1 = [ExpandedItem]()
                for moves in 0...validMoves.upperBound {
                    let expanded = ExpandedItem(
                        item: item, heading: direction, moves: moves
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
        let moves: Int
    }

    let items: [[Int]]
    let expandedItems: [[[[ExpandedItem]]]]
    let maxIndex: Index
    let validMoves: ClosedRange<Int>

    init(items: [[Int]], validMoves: ClosedRange<Int>) {
        self.items = items
        self.expandedItems = expand(items: items, validMoves: validMoves)
        self.validMoves = validMoves

        self.maxIndex = Index(
            xy: ComplexInt(x: items[0].count - 1, y: items.count - 1),
            heading: directions[maxDirection], moves: validMoves.upperBound)
    }

    var totalItems: Int {
        (maxIndex.xy.x + 1) * (maxIndex.xy.y + 1) *
        (maxDirection + 1) * (validMoves.upperBound + 1)
    }

    private func inBounds(u: Index) -> Bool {
        // assert(validMoves.contains(u.moves))
        u.xy.x >= 0 && u.xy.x <= maxIndex.xy.x &&
        u.xy.y >= 0 && u.xy.y <= maxIndex.xy.y &&
        validMoves.contains(u.moves)
    }

    func at(_ u: Index) -> ExpandedItem {
        let hi = directions.firstIndex(of: u.heading)!
        return expandedItems[u.xy.y][u.xy.x][hi][u.moves]
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

        let start = 1
        let end = validMoves.upperBound
        let inSameDirection: [Grid.Index] =
            if start <= end {
                 (start...end).map {
                    Index(xy: u.xy + $0 * h, heading: h, moves: u.moves + $0)
                }
            } else { [] }

        let turnStart = 1
        let turnEnd = validMoves.upperBound
        let afterTurning: [Grid.Index] =
            if turnStart <= turnEnd {
                 (turnStart...turnEnd).flatMap {
                    [
                        Index(xy: u.xy + $0 * hl, heading: hl, moves: $0),
                        Index(xy: u.xy + $0 * hr, heading: hr, moves: $0),
                    ]
                }
            } else { [] }

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
            for moves in 0...validMoves.upperBound {
                result.append(
                    Index(xy: xy, heading: direction, moves: moves)
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
    var inverseDistance = [0: Set([start])]
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
        // nextNearestD = Int.max
        // if let nn = nextNearest, !visited.contains(nn) { return nn }

        while let md = inverseDistance.keys.min(), let vs = inverseDistance[md] {
            for v in vs {
                if pending.contains(v) {
                    pending.remove(v)
                    return v
                }
            }
            inverseDistance[md] = nil
        }
        return nil
        // var u: Grid.Index?
        // var ud = Int.max
        // for v in pending {
        //     if let vd = distance[v], vd < ud {
        //         u = v
        //         ud = vd
        //     }
        // }
        // if let u { pending.remove(u) }
        // return u
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
                inverseDistance[du + w, default: Set()].insert(v)
                parent[v] = u
            }
            pending.insert(v)
        }
    }

    return state()
}

func trace(state: DijkstraState) {
    // if state.iteration % 500 == 0 {
    //     let total = state.grid.totalItems
    //     print("iteration \(state.iteration) found tentative distances to \(state.distance.count) / \(total) items")
    // }
}

extension Grid {
    /// Create string representation of the grid suitable for printing on a
    /// terminal.
    func renderToString(
        state: DijkstraState, start: Grid.Index, ends: Set<Grid.Index>
    ) -> String {
        let tHighlight = "\u{001B}[0;0m"
        let tDim = "\u{001B}[2;80m"
        let tReset = "\u{001B}[0m"

        let grid = state.grid
        let distance = state.distance

        // Trace the path back from the end to the start.
        var selectedPath = ends
        for end in ends {
            var u = end
            while let v = state.parent[u] {
                selectedPath.insert(v)
                u = v
            }
        }

        func pathInfo(xy: ComplexInt) -> (distance: Int, parentDirection: String, moves: Int)? {
            for u in grid.expandedIndex(xy: xy) {
                if selectedPath.contains(u) {
                    if let parent = parentDirection(u), let d = distance[u] {
                        return (distance: d, parentDirection: parent, moves: u.moves)
                    }
                }
            }
            return nil
        }

        func parentDirection(_ u: Index) -> String? {
            if u == start { return "○"}
            if ends.contains(u) { return "□"}
            guard let p = state.parent[u] else { fatalError() }
            switch(p.xy.x - u.xy.x, p.xy.y - u.xy.y) {
                case (let x, 0) where x < 0: return "→"
                case (let x, 0) where x > 0: return "←"
                case (0, let y) where y < 0: return "↓"
                case (0, let y) where y > 0: return "↑"
                default: fatalError()
            }
        }

        func pad3(_ s: String) -> String {
             String(("   " + s).suffix(3))
        }

        var result = [String]()
        let maxXY = grid.maxIndex.xy
        for y in 0...maxXY.y {
            for x in 0...maxXY.x {
                let xy = ComplexInt(x: x, y: y)
                let item = grid.at(xy: xy)
                if let (distance, parentDirection, moves) = pathInfo(xy: xy) {
                    let d = pad3("\(distance)")
                    result.append(tHighlight);
                    result.append("\(parentDirection) \(item) \(d) \(moves)  ")
                } else {
                    result.append(tDim)
                    result.append("  \(item)        ")
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

        let start = Grid.Index(xy: startXY, heading: heading, moves: 0)

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
        if let end {
            _ = end
            // print(
            //     grid.renderToString(state: state, start: start, ends: Set([end])),
            //     terminator: "")
        }
        return endDistance
    }

    return sp(heading: .east)
}

/// Reuse the function that shows the state of the grid after shortest path has
/// completed to show the neighbours of a particular item.
///
/// Useful for debugging.
func printNeighbours(_ u: Grid.Index, grid: Grid) {
    let neighbours = Set(grid.adjacent(u))

    var distance = [u: 0]
    var parent = [Grid.Index: Grid.Index]()

    for n in neighbours {
        distance[n] = 1
        parent[n] = u
    }

    let state = DijkstraState(
        grid: grid,
        iteration: 1,
        distance: distance,
        parent: parent
    )

    print("neighbours of \(u)")
    print(grid.renderToString(state: state, start: u, ends: neighbours),
          terminator: "")
}

/// We can move at most 3 blocks in a direction before we must turn.
let validMovesP1 = 0...3
/// We must move a minimum of 4 blocks before we can turn (or stop). We can move
/// a maximum of 10 blocks before we must turn.
let validMovesP2 = 4...10

/// A driver function
func sp(_ input: [[Int]], _ validMoves: ClosedRange<Int>) -> Int {
    let grid = Grid(items: input, validMoves: validMoves)
    return ourShortestPath(grid: grid) ?? -1
}

let input = readInput()
print(sp(input, validMovesP1), sp(input, validMovesP2))
