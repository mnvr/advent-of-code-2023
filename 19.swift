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

func readInput() -> (workflows: Workflows, parts: [Part]) {
    return ([:], [])
}
