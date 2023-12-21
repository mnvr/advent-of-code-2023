struct Module {
    enum MType {
        case flip
        case conjunction
        case broadcast
    }

    let type: MType?
    let inputs: [String]
    let outputs: [String]
}

extension Module: CustomStringConvertible {
    var description: String {
        let td = switch type {
            case .flip: "!"
            case .conjunction: "&"
            case .broadcast: "*"
            case nil: "#"
            }
        let id = inputs.joined(separator: "|")
        let od = outputs.joined(separator: "|")
        return [
            inputs.isEmpty ? nil : "\(id)>", td, outputs.isEmpty ? nil : ">\(od)"
        ].compactMap({ $0 }).joined(separator: "")
    }
}

typealias Modules = [String: Module]

let verbose = switch CommandLine.arguments.last {
    case "-v": 1
    case "-vv": 2
    default: 0
}

func readInput() -> Modules {
    // Pass 1: Build up partial modules
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
        modules[name] = Module(type: type, inputs: [], outputs: outputs)
    }

    // Discover all inputs
    var inputs = [String: Set<String>]()
    for (name, module) in modules {
        for output in module.outputs {
            inputs[output, default: Set()].insert(name)
            if modules[output] == nil {
                modules[output] = Module(type: nil, inputs: [], outputs: [])
            }
        }
    }

    // Pass 2: Hook up intputs into modules
    var result = Modules()
    for (name, module) in modules {
        let inputs = inputs[name]?.sorted() ?? []
        let outputs = module.outputs.sorted()
        result[name] = Module(type: module.type, inputs: inputs, outputs: outputs)
    }
    return result
}

// extension Module {
//     typealias State = [String: Bool]
//     typealias Pulse = (value: Bool, from: String, to: String)
// }

// typealias PropogateResult = (state: Module.State, pulses: [Module.Pulse])

// func propogate(
//     pulse: Module.Pulse, module: Module, state: Module.State
// ) -> PropogateResult? {
//     func emit(state: Module.State, value v: Bool) -> PropogateResult {
//         (state: state,
//          pulses: module.outputs.map { (value: v, from: module.name, to: $0) })
//     }

//     switch module.type {
//     case .broadcast:
//         return emit(state: state, value: pulse.value)
//     case .flip:
//         if pulse.value {
//             return nil
//         } else {
//             let newValue = !state["", default: false]
//             let newState = state.merging(["": newValue]) { _, new in new }
//             return emit(state: newState, value: newValue)
//         }
//     case .conjunction:
//         let newState = state.merging([pulse.from: pulse.value]) { _, new in new }
//         let newValue = !newState.values.reduce(true) { $0 && $1 }
//         return emit(state: newState, value: newValue)
//     default:
//         return nil
//     }
// }

// func simulate(modules: Modules, times: Int) -> (p1: Int, p2: Int) {
//     let buttonPress = (value: false, from: "button", to: "broadcaster")

//     var counts = [true: 0, false: 0]
//     // Examples don't have "rx", so don't go into an infinite loop. But since
//     // this solution doesn't work for p2 yet, this search for rx is actually
//     // always disabled for now.
//     var rxN: Int? = haveRx(modules: modules) ? 0 : 0
//     var states = [String: Module.State]()

//     initConjunctions(modules: modules, states: &states)

//     var n = 0
//     while n < times || rxN == nil {
//         n += 1

//         var pending = [buttonPress]
//         var pi = 0

//         counts[buttonPress.value]? += 1

//         while pi < pending.count {
//             let pulse = pending[pi]
//             let (value, from, to) = pulse
//             pi += 1

//             if verbose > 0 {
//                 print("button press \(n) pulse \(pi)\t\t\(from) -\(value ? "high" : "low")-> \(to)")
//             }

//             if to == "rx" {
//                 if !value {
//                     print("button press \(n) sending \(value) to rx")
//                     if rxN == nil || rxN == 0 {
//                         rxN = n
//                     }
//                 }
//             }

//             let module = modules[to, default: .named(to)]
//             let state = states[to, default: Module.State()]
//             if let new = propogate(pulse: pulse, module: module, state: state) {
//                 states[to] = new.state
//                 pending.append(contentsOf: new.pulses)
//                 for b in new.pulses.map({ $0.value }) {
//                     counts[b]? += 1
//                 }
//             }
//         }
//     }

//     let p1 = counts[false]! * counts[true]!
//     return (p1: p1, p2: rxN!)
// }


// func haveRx(modules: Modules) -> Bool {
//     for (input, module) in modules {
//         if input == "rx" { return true }
//         for output in module.outputs {
//             if output == "rx" { return true }
//         }
//     }
//     return false
// }

let modules = readInput()
print(modules)
// let r = simulate(modules: modules, times: 1000)
// print(r)
