var s = 0
var boxes: [[[String]]] = Array(repeating: [], count: 256)
while let line = readLine() {
    for word in line.split(separator: ",") {
        let splits = word.split { "=-".contains($0) } .map { String($0) }
        let bi = hash(splits[0])
        s += hash(String(word))
        modify(box: &boxes[bi], splits, bi)
    }
}
print(s, power(boxes))

func hash(_ s: String) -> Int {
    s.utf8.reduce(0) { ($0 + Int($1)) * 17 % 256 }
}

func modify(box: inout [[String]], _ splits: [String], _ bi: Int) {
    if splits.count == 1 { box.removeAll { $0[0] == splits[0] } }
    else {
        for (i, l) in box.enumerated() {
            if l[0] == splits[0] { return box[i] = splits  }
        }
        box.append(splits)
    }
}

func power(_ boxes: [[[String]]]) -> Int {
    boxes.enumerated().reduce(0) { s, b in
        b.1.enumerated().reduce(s) {
            $0 + (b.0 + 1) * ($1.0 + 1) * Int($1.1.last!)!
        }
    }
}
