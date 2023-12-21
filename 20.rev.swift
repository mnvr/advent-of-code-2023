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

typealias State = [String: Bool]

func initStates(modules: Modules) -> [String: State] {
    modules.compactMapValues { module in
        switch(module.type) {
            case .flip: ["": false]
            case .conjunction:
                Dictionary(uniqueKeysWithValues: module.inputs.map { ($0, false) })
            default: nil
        }
    }
}

struct Pulse {
    let value: Bool
    let from, to: String
}

extension Pulse: CustomStringConvertible {
    var description: String {
        return "\(from) -\(value ? "high" : "low")-> \(to)"
    }
}

func propogate(
    pulse: Pulse, module: Module, state: State?
) -> (state: State?, pulses: [Pulse])? {
     func emit(_ v: Bool) -> [Pulse] {
         module.outputs.map { Pulse(value: v, from: pulse.to, to: $0) }
     }

    switch module.type {
    case .broadcast:
        return (state, emit(pulse.value))
    case .flip:
        if pulse.value {
            return nil
        } else {
            let newValue = !state!["", default: false]
            let newState = state?.merging(["": newValue]) { _, new in new }
            return (newState, emit(newValue))
        }
    case .conjunction:
        let newState = state!.merging([pulse.from: pulse.value]) { _, new in new }
        let newValue = !newState.values.reduce(true) { $0 && $1 }
        return (newState, emit(newValue))
    case nil:
        return nil
    }
}

func simulate(modules: Modules, times: Int) -> Int {
    let buttonPress = Pulse(value: false, from: "button", to: "broadcaster")

    var counts = [true: 0, false: 0]
    func count(_ pulse: Pulse) {
        counts[pulse.value]? += 1
        if verbose > 0 {
            print(pulse)
        }
    }

    var states = initStates(modules: modules)
    show(modules: modules, states: states)

    var n = 0
    while n < times {
        n += 1

        var pending = [buttonPress]
        var pi = 0

        while pi < pending.count {
            let pulse = pending[pi]
            let to = pulse.to
            pi += 1
            count(pulse)

            let module = modules[to]!
            let state = states[to]
            if let new = propogate(pulse: pulse, module: module, state: state) {
                states[to] = new.state
                pending.append(contentsOf: new.pulses)
            }
        }
    }

    show(modules: modules, states: states)
    if verbose > 0 {
        print(counts)
    }

    return counts[false]! * counts[true]!
}

func show(modules: Modules, states: [String: State]) {
    if verbose > 0 {
        print(modules)
        print(states)
    }
}

func analyze(modules: Modules) -> Int {
    guard modules["rx"] != nil else { return 0 }

    var memo: [String: Int] = [:]
    func lowAfter(name: String) -> Int {
        if let m = memo[name] { return m }

        let r: Int

        let module = modules[name]!
        print("finding low after for \(name) \(module)")
        switch module.type {
        case nil:
            r = module.inputs.map({ lowAfter(name: $0) }).min()!
        case .broadcast:
            r = 1
        case .flip:
            r = module.inputs.map({ lowAfter(name: $0) }).min()!
        case .conjunction:
            r = module.inputs.map({ lowAfter(name: $0) }).min()!
        }

        memo[name] = r
        return r
    }

    let r = lowAfter(name: "rx")
    print(memo)
    return r
}

let modules = readInput()
// let p1 = simulate(modules: modules, times: 1000)
let p2 = analyze(modules: modules)
print(p2)
// print(p1, p2)
