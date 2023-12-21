struct Module {
    enum MType {
        case flip
        case conjunction
        case broadcast
    }

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
        modules[name] = Module(type: type, outputs: outputs)
    }
    return modules
}

let modules = readInput()
print(modules)
