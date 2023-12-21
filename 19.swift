typealias Workflows = [String: Workflow]

typealias Workflow = [Rule]

enum Attribute: String { case x, m, a, s }
enum Op { case lt, gt }
struct Condition {
    let attr: Attribute
    let attribute: Attribute
    let op: Op
    let num: Int

    var validRange: ClosedRange<Int> {
        switch op {
            case .lt: return 1...(num - 1)
            case .gt: return (num + 1)...4000
        }
    }
}

struct Rule {
    let condition: (Part) -> Bool
    let condition2: Condition?
    let action: Action
}

enum Action {
    case accept, reject
    case send(String)
}

struct Part {
    let x, m, a, s: Int
}

func readInput() -> (Workflows, [Part]) {
    var workflows: Workflows = [:]
    var parts: [Part]?
    while let line = readLine() {
        if line == "" {
            parts = readParts()
        } else {
            let (name, workflow) = parseWorkflow(line)
            workflows[name] = workflow
        }
    }
    return (workflows, parts!)
}

func parseWorkflow(_ line: String) -> (name: String, workflow: Workflow) {
    let splits = line.split { "{},".contains($0) }
    let name = String(splits[0])
    var workflow: Workflow = []
    for s in splits.dropFirst(1) {
        workflow.append(parseRule(s))
    }
    return (name: name, workflow: workflow)
}

func parseRule<S: StringProtocol>(_ s: S) -> Rule {
    let splits = s.split(separator: ":")
    if splits.count == 2 {
        return Rule(condition: parseCondition(splits[0]),
                    condition2: parseCondition2(splits[0]),
                    action: parseAction(splits[1]))
    } else {
        return Rule(condition: { _ in true }, condition2: nil, action: parseAction(splits[0]))
    }
}

func parseCondition<S: StringProtocol>(_ s: S) -> ((Part) -> Bool) {
    var splits = s.split(separator: "<")
    if splits.count > 1 {
        let num = Int(splits[1])!
        switch splits[0] {
        case "x": return { p in p.x < num }
        case "m": return { p in p.m < num }
        case "a": return { p in p.a < num }
        case "s": return { p in p.s < num }
        default: fatalError()
        }
    } else {
        splits = s.split(separator: ">")
        let num = Int(splits[1])!
        switch splits[0] {
        case "x": return { p in p.x > num }
        case "m": return { p in p.m > num }
        case "a": return { p in p.a > num }
        case "s": return { p in p.s > num }
        default: fatalError()
        }
    }
}

func parseCondition2<S: StringProtocol>(_ s: S) -> Condition {
    var op: Op

    var splits = s.split(separator: "<")
    if splits.count > 1 {
        op = .lt
    } else {
        splits = s.split(separator: ">")
        op = .gt
    }

    let attr = Attribute(rawValue: String(splits[0]))!
    let num = Int(splits[1])!

    return Condition(attr: attr, attribute: attr, op: op, num: num)
}

func parseAction<S: StringProtocol>(_ s: S) -> Action {
    switch s {
    case "A": return .accept
    case "R": return .reject
    default: return .send(String(s))
    }
}

func readParts() -> [Part] {
    var parts: [Part] = []
    while let line = readLine() {
        let n = line.split { !$0.isNumber }.map { Int($0)! }
        parts.append(Part(x: n[0], m: n[1], a: n[2], s: n[3]))
    }
    return parts
}

func process(workflows: Workflows, parts: [Part]) -> Int {
    parts.filter({ doesAccept(part: $0, workflows: workflows)}).map { p in
        p.x + p.m + p.a + p.s
    }.reduce(0, +)
}

func doesAccept(part: Part, workflows: Workflows) -> Bool {
    var workflowName = "in"
    while true {
        let workflow = workflows[workflowName]!
        // print("using workflow \(workflowName): \(workflow)")
        next: for rule in workflow {
            // print(part, workflowName, rule)
            if rule.condition(part) {
                // print("match")
                switch rule.action {
                    case .accept: return true
                    case .reject: return false
                    case .send(let wfn):
                        workflowName = wfn
                        break next
                }
            }
        }
    }
    // fatalError(workflowName)
    // return false
}

func filterRanges(workflows: Workflows) -> Int {
    func rangeCombinations(_ ranges: [Attribute: ClosedRange<Int>]) -> Int {
        ranges.values.reduce(1, { $0 * $1.count })
    }
    let attributes: [Attribute] = [.x, .m, .a, .s]
    let attributeRanges: [Attribute: ClosedRange<Int>] = [// Dictionary(uniqueKeysWithValues: attributes.map { ($0, 1...4000 })
        .x: 1...4000,
        .m: 1...4000,
        .a: 1...4000,
        .s: 1...4000
    ]
    var pending = [("in", attributeRanges)]
    var acceptedCount = 0

    nextPending: while let (workflow, attributeRanges) = pending.popLast() {
        print("processing attribute ranges \(attributeRanges) under workflow \(workflow)")
        for rule in workflows[workflow]! {
            if let condition = rule.condition2 {
                // Conditional rule, will cause attributeRanges to split.
                let range = attributeRanges[condition.attribute]!
                let newRange = range.clamped(to: condition.validRange)
                var newAttributeRanges = attributeRanges
                newAttributeRanges[condition.attribute] = newRange

                // Remember the parts that we didn't process here
                let remaining = remainingNonEmptyRanges(range: range, validRange: condition.validRange)
                for range in remaining {
                    newAttributeRanges[condition.attribute] = range
                    pending.append((workflow, attributeRanges))
                }

                // Process the part to which the condition applies
                switch rule.action {
                case .reject:
                    break
                case .accept:
                    acceptedCount += newAttributeRanges.reduce(1, { $0 * $1.value.count })
                case .send(let newWorkflow):
                    pending.append((newWorkflow, newAttributeRanges))
                }
            } else {
                // Unconditional rule, always a match, so applies to the whole ranges
                switch rule.action {
                case .reject:
                    break
                case .accept:
                    acceptedCount += attributeRanges.reduce(1, { $0 * $1.value.count })
                case .send(let newWorkflow):
                    pending.append((newWorkflow, attributeRanges))
                }
                // Done processing this rule
                continue nextPending
            }
        }
    }
    return acceptedCount
}

func remainingNonEmptyRanges(range: ClosedRange<Int>, validRange: ClosedRange<Int>
) -> [ClosedRange<Int>] {
    var result = [ClosedRange<Int>]()
    // Before valid range
    if range.lowerBound < validRange.lowerBound {
        result.append(range.lowerBound...(validRange.lowerBound - 1))
    }
    // After valid range
    if validRange.upperBound < range.upperBound {
        result.append((validRange.upperBound + 1)...range.upperBound)
    }
    return result
}

let (workflows, parts) = readInput()
// let p1 = process(workflows: workflows, parts: parts)
// print(p1)
let p2 = filterRanges(workflows: workflows)
print(p2)
