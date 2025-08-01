// Generated by Mochi transpiler v0.10.40 on 2025-07-25 21:09:22 GMT+7
import Foundation

var _nowSeed = 0
var _nowSeeded = false
func _now() -> Int {
    if !_nowSeeded {
        if let s = ProcessInfo.processInfo.environment["MOCHI_NOW_SEED"], let v = Int(s) {
            _nowSeed = v
            _nowSeeded = true
        }
    }
    if _nowSeeded {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        return _nowSeed
    }
    return Int(Date().timeIntervalSince1970 * 1_000_000_000)
}
func _mem() -> Int {
    if let status = try? String(contentsOfFile: "/proc/self/status") {
        for line in status.split(separator: "\n") {
            if line.hasPrefix("VmRSS:") {
                let parts = line.split(whereSeparator: { $0 == " " || $0 == "\t" })
                if parts.count >= 2, let kb = Int(parts[1]) {
                    return kb * 1024
                }
            }
        }
    }
    return 0
}
do {
    let _benchMemStart = _mem()
    let _benchStart = _now()
    indirect enum Beast {
        case Dog(kind: String, name: String)
        case Cat(kind: String, name: String)
    }
    func beastKind(_ b: Beast) -> String {
        return ({ () -> Any in
        switch b {
            case let .Dog(k, _):
            return k
            case let .Cat(k, _):
            return k
            default:
            var z: Any
            return z
        }
    }() as! String)
}
func beastName(_ b: Beast) -> String {
    return ({ () -> Any in
    switch b {
        case let .Dog(_, n):
        return n
        case let .Cat(_, n):
        return n
        default:
        var z: Any
        return z
    }
}() as! String)
}
func beastCry(_ b: Beast) -> String {
    return String(describing: { () -> String in
    switch b {
        case let .Dog(_, _):
        return "Woof"
        case let .Cat(_, _):
        return "Meow"
        default:
        var z: String
        return z
    }
}())
}
func bprint(_ b: Beast) {
    print((((((String(describing: beastName((b as! Beast))) + ", who's a ") + String(describing: beastKind((b as! Beast)))) + ", cries: \"") + String(describing: beastCry((b as! Beast)))) + "\"."))
}
func main() {
    let d: Beast = Beast.Dog(kind: "labrador", name: "Max")
    let c: Beast = Beast.Cat(kind: "siamese", name: "Sammy")
    _ = bprint((d as! Beast))
    _ = bprint((c as! Beast))
}
_ = main()
let _benchEnd = _now()
let _benchMemEnd = _mem()
print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
