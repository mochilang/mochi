// Generated by Mochi transpiler v0.10.50 on 2025-07-31 00:25:31 GMT+7
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
    func `repeat`(_ s: String, _ n: Int) -> String {
        var out: String = ""
        var i: Int = 0
        while (i < n) {
            out = ((out + s) as! String)
            i = ((i + 1) as! Int)
        }
        return (out as! String)
    }
    var records: [String] = ([String(describing: `repeat`("abcdefgh", 10)), String(describing: `repeat`("ijklmnop", 10)), String(describing: `repeat`("qrstuvwx", 10))] as! [String])
    func reverseStr(_ s: String) -> String {
        var out: String = ""
        var i: Int = (Int(((s).count)) - 1)
        while (i >= 0) {
            out = ((out + String(Array(s)[i..<(i + 1)])) as! String)
            i = ((i - 1) as! Int)
        }
        return (out as! String)
    }
    for r in records {
        print(_p(String(describing: reverseStr((r as! String)))))
    }
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
