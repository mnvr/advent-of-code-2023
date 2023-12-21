struct Module {
    enum MType {
        case flip
        case conjunction
        case broadcast
    }

    let name: String
    let type: MType?
    let outputs: [String]

    static func named(_ name: String) -> Module {
        Module(name: name, type: nil, outputs: [])
    }
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
    typealias Pulse = (value: Bool, from: String, to: String)
}

typealias PropogateResult = (state: Module.State, pulses: [Module.Pulse])

func propogate(
    pulse: Module.Pulse, module: Module, state: Module.State
) -> PropogateResult? {
    func emit(state: Module.State, value v: Bool) -> PropogateResult {
        (state: state,
         pulses: module.outputs.map { (value: v, from: module.name, to: $0) })
    }

    switch module.type {
    case .broadcast:
        return emit(state: state, value: pulse.value)
    case .flip:
        if pulse.value {
            return nil
        } else {
            let newValue = !state["", default: false]
            let newState = state.merging(["": newValue]) { _, new in new }
            return emit(state: newState, value: newValue)
        }
    case .conjunction:
        let newState = state.merging([pulse.from: pulse.value]) { _, new in new }
        let newValue = !newState.values.reduce(true) { $0 && $1 }
        return emit(state: newState, value: newValue)
    default:
        return nil
    }
}

func simulate(modules: Modules, times: Int) -> (counts: [Bool: Int], result: Int, countTillRx: Int) {
    let buttonPress = (value: false, from: "button", to: "broadcaster")

    var counts = [true: 0, false: 0]
    // Examples don't have "rx", so don't go into an infinite loop.
    var countTillRx: Int? = haveRx(modules: modules) ? nil : 0
    var states = [String: Module.State]()

    initConjunctions(modules: modules, states: &states)

    var c = 0
    while c < times || countTillRx == nil {
        c += 1

        if c == 100_000 { // TODO: temporary
             countTillRx = 0
        }

        var pending = [buttonPress]
        var pi = 0

        counts[buttonPress.value]? += 1

        while pi < pending.count {
            let pulse = pending[pi]
            let (value, _, to) = pulse
            pi += 1

            if to == "rx" {
                print("at count \(c) sending \(value) to rx")
                if !value {
                    countTillRx = c
                }
            }

            let module = modules[to, default: .named(to)]
            let state = states[to, default: Module.State()]
            if let new = propogate(pulse: pulse, module: module, state: state) {
                states[to] = new.state
                pending.append(contentsOf: new.pulses)
                for b in new.pulses.map({ $0.value }) {
                    counts[b]? += 1
                }
            }
        }
    }

    let result = counts[false]! * counts[true]!
    return (counts, result, countTillRx: countTillRx!)
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
            let initialState = inputs.map { ($0, false) }
            states[name] = Dictionary(uniqueKeysWithValues: initialState)
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

let modules = readInput()
let p1 = simulate(modules: modules, times: 1000)
print(p1)
// print(p2)
// print(p1.result)
