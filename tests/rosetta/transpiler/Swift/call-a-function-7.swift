// Generated by Mochi transpiler v0.10.50 on 2025-07-31 08:10:08 GMT+7
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
    func f() -> [Any?] {
        return ([(0 as! Any), (0.0 as! Any)] as! [Any])
    }
    func g(_ a: Int, _ b: Double) -> Int {
        return 0
    }
    func h(_ s: String, _ nums: [Int]) {
    }
    func main() {
        if (((2 * Int(g(1, 3.0))) + 4) > 0) {
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
