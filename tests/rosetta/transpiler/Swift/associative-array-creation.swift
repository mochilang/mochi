// Generated by Mochi transpiler v0.10.40 on 2025-07-26 10:25:15 GMT+7
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
    func removeKey(_ m: [String: Int], _ k: String) -> [String: Int] {
        var out: [String: Int] = ([:] as! [String: Int])
        for key in m.keys.sorted() {
            if (key != k) {
                out[key] = m[key]
            }
        }
        return (out as! [String: Int])
    }
    func main() {
        var x: [String: Int] = ([:] as! [String: Int])
        x = ([:] as! [String: Int])
        x["foo"] = 3
        let y1 = x["bar"]
        let ok = (x["bar"] != nil)
        print(_p(y1))
        print(_p(ok))
        x = (removeKey((x as! [String: Int]), "foo") as! [String: Int])
        x = (["foo": 2, "bar": 42, "baz": -1] as [String: Any] as! [String: Int])
        print(_p(x["foo"]), _p(x["bar"]), _p(x["baz"]))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
