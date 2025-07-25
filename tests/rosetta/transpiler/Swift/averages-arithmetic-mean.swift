// Generated by Mochi transpiler v0.10.40 on 2025-07-26 10:32:27 GMT+7
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
    func mean(_ v: [Double]) -> [String: Any] {
        if (Int(((v).count)) == 0) {
            return (["ok": false] as! [String: Any])
        }
        var sum = 0.0
        var i = 0
        while (i < Int(((v).count))) {
            sum = Double((sum + v[i]))
            i = Int((i + 1))
        }
        return (["ok": true, "mean": (sum / Double(((v).count)))] as! [String: Any])
    }
    func main() {
        let sets = [[], ([3.0, 1.0, 4.0, 1.0, 5.0, 9.0] as! [Double]), ([100000000000000000000.0, 3.0, 1.0, 4.0, 1.0, 5.0, 9.0, -100000000000000000000.0] as! [Double]), ([10.0, 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.11] as! [Double]), ([10.0, 20.0, 30.0, 40.0, 50.0, -100.0, 4.7, -1100.0] as! [Double])]
        for v in sets {
            print(_p(("Vector: " + _p(v))))
            let r = (mean((v as! [Double])) as! [String: Any])
            if ((r as! [String: Any])["ok"] as! Bool) {
                print(_p(((("Mean of " + _p(Int(((v).count)))) + " numbers is ") + _p((r as! [String: Any])["mean"]))))
            } else {
                print(_p("Mean undefined"))
            }
            print(_p(""))
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
