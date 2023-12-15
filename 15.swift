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
    case remove(String)
    case replace(Lens)
}

struct Lens {
    let label: String
    let value: Int

    init<S: StringProtocol>(label: S, value: S) {
        self.label = String(label)
        self.value = Int(value, radix: 10)!
    }
}

struct Action {
    let op: Op
    let box: Int

    init(_ op: Op, box: Int) {
        self.op = op
        self.box = box
    }
}

func decode<S: StringProtocol>(_ s: S) -> Action {
    let splits = s.split { $0 == "=" || $0 == "-" }
    let box = hash(splits[0])
    if splits.count == 1 {
        return Action(.remove(String(splits[0])), box: box)
    }
    return Action(.replace(Lens(label: splits[0], value: splits[1])), box: box)
}
