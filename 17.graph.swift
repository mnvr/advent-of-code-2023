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
    }

    let items: [[T]]
    let maxIndex: Index

    init(items: [[T]]) {
        self.items = items
        self.maxIndex = Index(x: items[0].count, y: items.count)
    }
}

func dfs<T>(grid: Grid<T>, start: Grid<T>.Index, visit: (T) -> Void) {
    // var visited = Set<Grid<T>.Index>()
    print("helol")
}

let input = readInput()
let grid = Grid(items: input)
print(grid)
dfs(grid: grid, start: .init(x: 0, y: 0)) { print($0) }
