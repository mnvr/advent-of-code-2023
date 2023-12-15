var v = 0
while let line = readLine() {
    for word in line.split(separator: " ") {
        for c in word.utf8 {
            print(c, v)
            v += Int(c)
            v *= 17
            v %= 256
        }
    }
    print(line, line.count, v)
}
