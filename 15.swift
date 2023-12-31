var s = 0
var boxes: [Box] = Array(repeating: [], count: 256)
while let line = readLine() {
    for word in line.split(separator: ",") {
        s += hash(word)
        boxes = modify(boxes, step: decode(word))
    }
}
print(s, power(boxes))

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
    let length: Int

    init<S: StringProtocol>(label: S, length: S) {
        self.label = String(label)
        self.length = Int(length, radix: 10)!
    }
}

typealias Box = [Lens]

struct Step {
    let op: Op
    let box: Int
}

func decode<S: StringProtocol>(_ s: S) -> Step {
    let splits = s.split { $0 == "=" || $0 == "-" }
    let box = hash(splits[0])
    if splits.count == 1 {
        return Step(op: .remove(String(splits[0])), box: box)
    }
    let lens = Lens(label: splits[0], length: splits[1])
    return Step(op: .replace(lens), box: box)
}

func modify(_ boxes: [Box], step: Step) -> [Box] {
    var boxes = boxes;
    boxes[step.box] = modify(box: boxes[step.box], step: step)
    return boxes
}

func modify(box: Box, step: Step) -> Box {
    var box = box
    switch step.op {
        case .remove(let label):
            box.removeAll { $0.label == label }
        case .replace(let lens):
            var found = false
            for (i, l) in box.enumerated() {
                if l.label == lens.label {
                    box[i] = lens
                    found = true
                    break
                }
            }
            if !found {
                box.append(lens)
            }
    }
    return box
}

func power(_ boxes: [Box]) -> Int {
    var s = 0
    for (i, box) in boxes.enumerated() {
        for (j, lens) in box.enumerated() {
            s += (i + 1) * (j + 1) * lens.length
        }
    }
    return s
}
