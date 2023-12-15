var s = 0
while let line = readLine() {
    for word in line.split(separator: ",") {
        s += hash(word)
        print(decode(word))
    }
}
// print(s)

func hash<S: StringProtocol>(_ s: S) -> Int {
    var v = 0;
    for c in s.utf8 {
        v += Int(c)
        v *= 17
        v %= 256
    }
    return v;
}

enum Op {
    case remove
    case replace(Int)
}

struct Action {
    let box: Int
    let label: String
    let op: Op

    init<S: StringProtocol>(label: S, op: Op) {
        self.box = hash(label)
        self.label = String(label)
        self.op = op
    }
}

func decode<S: StringProtocol>(_ s: S) -> Action {
    let splits = s.split { $0 == "=" || $0 == "-" }
    if splits.count == 1 {
        return Action(label: splits[0], op: .remove)
    }
    return Action(label: splits[0], op: .replace(Int(splits[1], radix: 10)!))
}
