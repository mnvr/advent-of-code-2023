struct Module {
    enum MType {
        case flip
        case conjunction
        case broadcast
    }

    let name: String
    let type: MType?
    let outputs: [String]
}

typealias Modules = [String: Module]

func readInput() -> Modules {
    var modules = Modules()
    while let line = readLine() {
        let words = line.split { !$0.isLetter }
        let type: Module.MType? = switch line.first {
            case "%": .flip
            case "&": .conjunction
            default: words.first == "broadcaster" ? .broadcast : nil
        }
        let name = String(words.first!)
        let outputs = Array(words.dropFirst(1).map { String($0) })
        modules[name] = Module(name: name, type: type, outputs: outputs)
    }
    return modules
}

extension Module {
    typealias State = [String: Bool]
    typealias Ping = (pulse: Bool, from: String)
    typealias Emittance = (ping: Ping, to: String)
}

typealias PropogateResult = (state: Module.State, emits: [Module.Emittance])

func propogate(
    ping: Module.Ping, module: Module, state: Module.State
) -> PropogateResult? {
    let name = module.name
    func emit(_ pulse: Bool, state _s: Module.State? = nil) -> PropogateResult {
        (state: _s ?? state,
         emits: module.outputs.map { ((pulse: pulse, from: name), to: $0) })
    }

    switch module.type {
    case .broadcast:
        return emit(ping.pulse)
    case .flip:
        if ping.pulse {
            return nil
        } else {
            let pulse = !state["", default: false]
            let newState = state.merging(["": pulse]) { _, new in new }
            return emit(pulse, state: newState)
        }
    case .conjunction:
        var newState = state
        newState[ping.from] = ping.pulse
        return emit(!newState.values.reduce(true, { $0 && $1 }), state: newState)
    default:
        return nil
    }
}

func initConjunctions(modules: Modules, states: inout [String: Module.State]) {
    var inputs = [String: Set<String>]()

    for (input, module) in modules {
        for output in module.outputs {
            inputs[output, default: Set()].insert(input)
        }
    }

    for (name, module) in modules {
        if module.type == .conjunction, let inputs = inputs[name] {
            states[name] = Dictionary(uniqueKeysWithValues: inputs.map { ($0, false)} )
        }
    }
}

func haveRx(modules: Modules) -> Bool {
    for (input, module) in modules {
        if input == "rx" { return true }
        for output in module.outputs {
            if output == "rx" { return true }
        }
    }
    return false
}

func simulate(modules: Modules, times: Int) -> (counts: [Bool: Int], result: Int, countTillRx: Int) {
    let buttonPress = (ping: (pulse: false, from: "button"), to: "broadcaster")

    var counts = [Bool: Int]()
    // Examples don't have "rx", so don't go into an infinite loop.
    var countTillRx: Int? = haveRx(modules: modules) ? nil : 0
    var states = [String: Module.State]()

    initConjunctions(modules: modules, states: &states)

    var i = 0
    while i < times || countTillRx == nil {
        i += 1

        var pending = [buttonPress]
        counts[buttonPress.ping.pulse] = counts[buttonPress.ping.pulse, default: 0] + 1
        while let e = pending.popLast() {
            print(e)
            let destination = modules[e.to, default: Module(name: e.to, type: nil, outputs: [])]
            let state = states[e.to, default: Module.State()]
            if let change = propogate(ping: e.ping, module: destination, state: state) {
                states[e.to] = change.state
                pending.append(contentsOf: change.emits)
                for b in change.emits.map({ $0.ping.pulse }) {
                    counts[b] = counts[b, default: 0] + 1
                }
                if countTillRx == nil {
                    for c in change.emits {
                        if c.to == "rx" && !c.ping.pulse {
                            countTillRx = i
                            break
                        }
                    }
                }
            }
        }
    }

    let result = counts[false, default: 0] * counts[true, default: 0]
    return (counts, result, countTillRx: countTillRx!)
}


func simulateP2(modules: Modules) -> Int {
    // Examples don't have "rx", so don't go into an infinite loop.
    if (!haveRx(modules: modules)) {
        return 0
    }

    let buttonPress = (ping: (pulse: false, from: "button"), to: "broadcaster")

    var counts = [Bool: Int]()
    var states = [String: Module.State]()

    initConjunctions(modules: modules, states: &states)

    var c = 0
    while true {
        c += 1

        var pending = [buttonPress]
        counts[buttonPress.ping.pulse] = counts[buttonPress.ping.pulse, default: 0] + 1
        while let e = pending.popLast() {
            if e.to == "rx" {
                print("at count \(c) sending \(e.ping.pulse) to rx")
                if !e.ping.pulse {
                    return c
                }
            }

            let destination = modules[e.to, default: Module(name: e.to, type: nil, outputs: [])]
            let state = states[e.to, default: Module.State()]
            if let change = propogate(ping: e.ping, module: destination, state: state) {
                states[e.to] = change.state
                pending.append(contentsOf: change.emits)
                for b in change.emits.map({ $0.ping.pulse }) {
                    counts[b] = counts[b, default: 0] + 1
                }
            }
        }
    }
}

let modules = readInput()
let p1 = simulate(modules: modules, times: 1000)
// let p2 = simulateP2(modules: modules)
print(p1)
// print(p2)
// print(p1.result)
