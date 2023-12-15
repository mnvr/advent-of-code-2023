while let line = readLine() {
    for word in line.split(separator: " ") {
        for v in word.utf8 {
            print(v)
        }
    }
    print(line, line.count)
}
