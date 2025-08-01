// Generated by Mochi transpiler v0.10.41 on 2025-07-27 01:19:41 GMT+7
import Foundation

func _p(_ v: Any?) -> String {
    if let val = v { return String(describing: val) }
    return "<nil>"
}

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
    func parseBool(_ s: String) -> Bool {
        let l = String(describing: (s.lowercased()))
        if (((((l == "1") || (l == "t")) || (l == "true")) || (l == "yes")) || (l == "y")) {
            return true
        }
        return false
    }
    func main() {
        var n: Bool = true
        print(_p(n))
        print(_p("bool"))
        n = ((!n) as! Bool)
        print(_p(n))
        let x = 5
        let y = 8
        print(_p("x == y:"), _p(((x == y) ? 1 : 0)))
        print(_p("x < y:"), _p(((x < y) ? 1 : 0)))
        print(_p("\nConvert String into Boolean Data type\n"))
        let str1 = "japan"
        print(_p("Before :"), _p("string"))
        let bolStr = (parseBool(String(str1)) as! Bool)
        print(_p("After :"), _p("bool"))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
