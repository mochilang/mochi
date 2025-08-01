// Generated by Mochi transpiler v0.10.40 on 2025-07-26 04:52:59 GMT+7
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
    func fib(_ n: Int) -> Int {
        if (n < 2) {
            return Int(n)
        }
        var a = 0
        var b = 1
        var i = 1
        while (i < n) {
            let t = (a + b)
            a = Int(b)
            b = Int(t)
            i = Int((i + 1))
        }
        return Int(b)
    }
    func main() {
        for n in ([0, 1, 2, 3, 4, 5, 10, 40, -1] as! [Int]) {
            if (n < 0) {
                print("fib undefined for negative numbers")
            } else {
                print(((("fib " + String(describing: n)) + " = ") + String(describing: Int(fib(Int(n))))))
            }
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
