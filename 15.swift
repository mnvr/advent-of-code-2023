var s = 0
var boxes: [Box] = Array(repeating: [], count: 256)
while let line = readLine() {
    for word in line.split(separator: ",") {
        s += hash(word)
        // print(decode(word))
        boxes = modify(boxes, action: decode(word))
    }
}
// print(s)
print(boxes)

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

typealias Box = [Lens]

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

func modify(_ boxes: [Box], action: Action) -> [Box] {
    var boxes = boxes;
    boxes[action.box] = modify(box: boxes[action.box], action: action)
    return boxes
}

func modify(box: Box, action: Action) -> Box {
    var box = box
    switch action.op {
        case .remove(let label):
            box.removeAll(where: { $0.label == label })
        case .replace(let lens):
            var found = false
            for (i, l) in box.enumerated() {
                if l.label == lens.label {
                    box[i] = lens
                    found = true
                }
            }
            if !found {
                box.append(lens)
            }
    }
    return box
}
