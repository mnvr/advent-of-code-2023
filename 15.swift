var s = 0
while let line = readLine() {
    for word in line.split(separator: ",") {
        s += hash(word)
    }
}
print(s)

func hash<S: StringProtocol>(_ s: S) -> Int {
    var v = 0;
    for c in s.utf8 {
        v += Int(c)
        v *= 17
        v %= 256
    }
    return v;
}
