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
            var newState = state
            newState[""] = pulse
            return emit(pulse, state: newState)
        }
    case .conjunction:
        var newState = state
        newState[ping.from] = ping.pulse
        return emit(!state.values.reduce(true, { $0 && $1 }), state: newState)
    default:
        return nil
    }
}

func simulate(modules: Modules) -> (counts: [Bool: Int], dummy: Bool) {
    var counts = [Bool: Int]()
    var states = [String: Module.State]()
    let buttonPress = (ping: (pulse: false, from: "button"), to: "broadcaster")
    var pending = [buttonPress]
    while let e = pending.popLast() {
        let destination = modules[e.to]!
        let state = states[e.to, default: Module.State()]
        if let change = propogate(ping: e.ping, module: destination, state: state) {
            states[e.to] = change.state
            pending.append(contentsOf: change.emits)
            for b in change.emits.map({ $0.ping.pulse }) {
                counts[b] = counts[b, default: 0] + 1
            }
        }
    }
    return (counts, true)
}

let modules = readInput()
let (counts, _) = simulate(modules: modules)
print(counts)
