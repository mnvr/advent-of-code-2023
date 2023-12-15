var s = 0
var boxes: [[[String]]] = Array(repeating: [], count: 256)
while let line = readLine() {
    for word in line.split(separator: ",") {
        let splits = word.split { $0 == "=" || $0 == "-" } .map { String($0) }
        let bi = hash(splits[0])
        s += hash(String(word))
        boxes[bi] = modify(box: boxes[bi], splits, bi)
    }
}
print(s, power(boxes))

func hash(_ s: String) -> Int {
    return s.utf8.reduce(0) { ($0 + Int($1)) * 17 % 256 }
}

func modify(box: [[String]], _ splits: [String], _ bi: Int) -> [[String]] {
    var box = box
    var label = splits.first
    if splits.count == 1 {
        box.removeAll { $0.first == label }
    } else {
        for (i, l) in box.enumerated() {
            if l.first == label {
                box[i] = splits
                label = nil
            }
        }
        if let _ = label {
            box.append(splits)
        }
    }
    return box
}

func power(_ boxes: [[[String]]]) -> Int {
    var s = 0
    for (i, box) in boxes.enumerated() {
        for (j, lens) in box.enumerated() {
            s += (i + 1) * (j + 1) * Int(lens.last!, radix: 10)!
        }
    }
    return s
}
