typealias Workflows = [String: Workflow]

typealias Workflow = [Rule]

struct Rule {
    let condition: ((Part) -> Bool)?
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
                    action: parseAction(splits[1]))
    } else {
        return Rule(condition: nil, action: parseAction(splits[0]))
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

let input = readInput()
print(input)
