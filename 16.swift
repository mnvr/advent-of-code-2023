var grid: Grid = []
while let line = readLine() {
    grid.append(line)
}
print(grid)

typealias Grid = [String]
typealias Ix = (Int, Int)
enum Direction {
    case l, r, u, d
}
typealias Beam = (Ix, Direction)

// func trace(beam: )
