enum Module {
    case flip(String, [Int], UInt8)
    case conjunction(String, Int, [Int], UInt64)
    case broadcast(String, [Int])
    case other(String)
}

extension Module: CustomStringConvertible {
    var description: String {
        func pad2(_ n: Int) -> String {
            n < 10 ? "0\(n)" : "\(n)"
        }

        func outs(_ outputs: [Int]) -> String {
            outputs.sorted().map { pad2($0) }.joined(separator: "|")
        }

        switch self {
        case .flip(let name, let outputs, _):
            return "\(name):\(stateString!)>\(outs(outputs))"
        case .conjunction(let name, _, let outputs, _):
            return "\(name):\(stateString!)>\(outs(outputs))"
        case .broadcast(let name, let outputs):
            return "\(name):>\(outs(outputs))"
        case .other(let name):
            return name
        }
    }

    var stateString: String? {
        switch self {
        case .conjunction(_, let inputCount, _, let state):
            String(String(state, radix: 2).suffix(inputCount))
        case .flip(_, _, let state): state == 1 ? "1" : "0"
        default: nil
        }
    }
}

var modulesIndexToName: [Int: String] = [:]
var rxIndex: Int?
let verbose = switch CommandLine.arguments.last {
    case "-v": 1
    case "-vv": 2
    case "-vvv": 3
    default: 0
}


func readInput() -> [Module] {
    let broadcast = "broadcaster"

    var modules = [String: (partial: Module, outputs: [String])]()
    while let line = readLine() {
        let words = line.split { !$0.isLetter }
        let name = String(words.first!)
        let partial: Module? = switch line.first {
            case "%": .flip("", [], 0)
            case "&": .conjunction("", 0, [], 0)
            default: name == broadcast ? .broadcast("", []) : .other(name)
        }
        let outputs = Array(words.dropFirst(1).map { String($0) })
        modules[name] = (partial!, outputs)
    }

    var uniqueNames = Set(modules.keys)

    var inputs = [String: Set<String>]()
    for (name, parsed) in modules {
        for output in parsed.outputs {
            uniqueNames.insert(output)
            inputs[output, default: Set()].insert(name)
        }
    }

    // Ensure broadcast is at index 0
    uniqueNames.remove(broadcast)
    let keys = [broadcast] + Array(uniqueNames)

    var result = [Module]()
    for name in keys {
        let parsed = modules[name]
        let js = parsed?.outputs.map { keys.firstIndex(of: $0)! }
        switch parsed?.partial {
        case .broadcast:
            result.append(.broadcast(name, js!))
        case .other:
            result.append(.other(name))
        case nil:
            result.append(.other(name))
        case .flip:
            result.append(.flip(name, js!, 0))
        case .conjunction:
            var state = UInt64.max
            let ins = inputs[name]!
            for input in ins {
                let k = keys.firstIndex(of: input)!
                state ^= (1 << k)
            }
            result.append(.conjunction(name, ins.count, js!, state))
        }

        modulesIndexToName[result.count - 1] = name
        if name == "rx" {
            rxIndex = result.count - 1
        }
    }

    return result
}

struct Pulse {
    let value: Bool
    let from: Int
    let to: Int
}

extension Pulse: CustomStringConvertible {
    var description: String {
        let f = from == -1 ? "button" : modulesIndexToName[from]!
        let t = modulesIndexToName[to]!
        let v = value ? "high" : "low"
        return "\(f) -\(v)-> \(t)"
    }
}

func propogate(pulse: Pulse, module: Module) -> ([Pulse], Module)? {
    func emit(_ value: Bool, _ outputs: [Int]) -> [Pulse] {
        outputs.map { Pulse(value: value, from: pulse.to, to: $0 ) }
    }

    func toggle(_ s: UInt8) -> UInt8 {
        s == 0 ? 1 : 0
    }

    switch module {
    case .other: return nil
    case .broadcast(_, let outputs): return (emit(pulse.value, outputs), module)
    case .flip(let name, let outputs, var state):
        if pulse.value { return nil }
        else {
            state = toggle(state)
            return (emit(state == 1, outputs), .flip(name, outputs, state))
        }
    case .conjunction(let name, let n, let outputs, var state):
        if pulse.value {
            state |= (1 << pulse.from)
        } else {
            state &= ~(1 << pulse.from)
        }
        let value = state != .max
        return (emit(value, outputs), .conjunction(name, n, outputs, state))
    }
}

func simulate(modules: inout [Module]) -> (Int, Int) {
    let buttonPress = Pulse(value: false, from: -1, to: 0)


    var (ct, cf) = (0, 0)
    func count(_ pulse: Pulse) {
        if (verbose > 2) {
            print(pulse)
        }
        if pulse.value { ct += 1 }
        else { cf += 1 }
    }

    var n = 0
    var rxn: Int? = rxIndex == nil ? 0 : nil
    while n < 1000 || rxn == nil {
        n += 1
        show(modules: modules)

        var pending = [buttonPress]
        var pi = 0
        while pi < pending.count {
            let pulse = pending[pi]
            count(pulse)
            pi += 1

            if (pulse.to == rxIndex && !pulse.value) {
                rxn = n
            }

            let module = modules[pulse.to]
            if let (pulses, newModule) = propogate(pulse: pulse, module: module) {
                modules[pulse.to] = newModule
                pending.append(contentsOf: pulses)
            }
        }
    }

    if verbose > 1 {
        print("counts", ct, cf)
    }
    return (ct * cf, rxn ?? 0)
}

func show(modules: [Module]) {
    if verbose > 1 {
        print(modules.map({ $0.description }).joined(separator: " · "))
    } else if verbose > 0 {
        print(modules.compactMap({ $0.stateString }).joined(separator: "·"))
    }
}

var modules = readInput()
let p1 = simulate(modules: &modules)
show(modules: modules)
print(p1)
