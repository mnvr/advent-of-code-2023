typealias Workflows = [String: Workflow]

typealias Workflow = [Rule]

enum Attribute: String { case x, m, a, s }

enum Op { case lt, gt }

struct Condition {
    let attribute: Attribute
    let op: Op
    let num: Int

    func isValid(part: Part) -> Bool {
        switch attribute {
            case .x: isValid(part.x)
            case .m: isValid(part.m)
            case .a: isValid(part.a)
            case .s: isValid(part.s)
        }
    }

    private func isValid(_ x: Int) -> Bool {
        switch op {
            case .lt: x < num
            case .gt: x > num
        }
    }

    var validRange: ClosedRange<Int> {
        switch op {
            case .lt: 1...(num - 1)
            case .gt: (num + 1)...4000
        }
    }
}

struct Rule {
    let condition: Condition?
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
    let sp = s.split(separator: ":")
    if sp.count == 2 {
        return Rule(condition: parseCondition(sp[0]), action: parseAction(sp[1]))
    } else {
        return Rule(condition: nil, action: parseAction(sp[0]))
    }
}

func parseCondition<S: StringProtocol>(_ s: S) -> Condition {
    var splits = s.split(separator: "<")
    var op: Op
    if splits.count > 1 {
        op = .lt
    } else {
        splits = s.split(separator: ">")
        op = .gt
    }

    let attribute = Attribute(rawValue: String(splits[0]))!
    let num = Int(splits[1])!

    return Condition(attribute: attribute, op: op, num: num)
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
    next: while true {
        let workflow = workflows[workflowName]!
        for rule in workflow {
            if let condition = rule.condition, !condition.isValid(part: part) {
            } else {
                switch rule.action {
                    case .accept: return true
                    case .reject: return false
                    case .send(let wfn):
                        workflowName = wfn
                        continue next
                }
            }
        }
    }
}

func filterRanges(workflows: Workflows) -> Int {
    let attributes: [Attribute] = [.x, .m, .a, .s]
    let attributeRanges = Dictionary(uniqueKeysWithValues: attributes.map { ($0, 1...4000) })
    var pending = [("in", attributeRanges)]
    var acceptedCount = 0

    nextPending: while let (workflow, _attributeRanges) = pending.popLast() {
        var attributeRanges = _attributeRanges
        for rule in workflows[workflow]! {
            if let condition = rule.condition {
                // Conditional rule, will cause attributeRanges to split.

                // Find the part to which this rule applies.
                let newAttributeRanges = transformed(
                    attributeRanges: attributeRanges,
                    attribute: condition.attribute) {
                    $0.clamped(to: condition.validRange)
                }

                // Process the part to which the condition applied.
                switch rule.action {
                case .reject:
                    break
                case .accept:
                    acceptedCount += combinations(attributeRanges: newAttributeRanges)
                case .send(let newWorkflow):
                    pending.append((newWorkflow, newAttributeRanges))
                }

                // For the parts that we didn't process here
                let remaining = remainingNonEmptyRanges(
                    range: attributeRanges[condition.attribute]!,
                    validRange: condition.validRange)
                // If there are zero of them, then we're done with this pending
                // entry
                if remaining.count == 0 { continue nextPending }
                // If there is one of them, that continues on with the sequence
                // of rules in this workflow.
                if remaining.count == 1 {
                    attributeRanges[condition.attribute] = remaining[0]
                } else {
                    // 2 of them. Haven't handled this case yet
                    fatalError("found 2 remaining entries \(remaining)")
                }
            } else {
                // Unconditional rule, always a match, so applies to the whole ranges
                switch rule.action {
                case .reject:
                    break
                case .accept:
                    acceptedCount += combinations(attributeRanges: attributeRanges)
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

typealias AttributeRanges = [Attribute: ClosedRange<Int>]

func combinations(attributeRanges: AttributeRanges) -> Int {
    attributeRanges.values.reduce(1, { $0 * $1.count })
}

func transformed(
    attributeRanges:AttributeRanges, attribute: Attribute,
    transform: (ClosedRange<Int>) -> ClosedRange<Int>
) -> AttributeRanges {
    var copy = attributeRanges
    copy[attribute] = transform(attributeRanges[attribute]!)
    return copy
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
let p1 = process(workflows: workflows, parts: parts)
let p2 = filterRanges(workflows: workflows)
print(p1, p2)
