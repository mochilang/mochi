// Generated by Mochi transpiler v0.10.50 on 2025-07-31 08:05:04 GMT+7
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
func _append<T>(_ xs: [T], _ v: T) -> [T] {
    var out = xs
    out.append(v)
    return out
}
do {
    let _benchMemStart = _mem()
    let _benchStart = _now()
    func main() {
        var list: [Int] = ([] as! [Int])
        var a: Int = 1
        var d: Int = 2
        var e: Int = 3
        var i: Int = 4
        list = (_append(list, a) as! [Int])
        list = (_append(list, d) as! [Int])
        list = (_append(list, e) as! [Int])
        list = (_append(list, i) as! [Int])
        i = Int(((list).count))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
