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
    return Grid(items: items, lastNode: .init(x: x, y: y))
}

let grid = readInput()
print(grid)
