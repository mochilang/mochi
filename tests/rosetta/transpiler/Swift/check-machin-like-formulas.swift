// Generated by Mochi transpiler v0.10.50 on 2025-07-31 08:16:46 GMT+7
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
    func br(_ n: Int, _ d: Int) -> Double {
        return Double(((n as! Double) / (d as! Double)))
    }
    var testCases: [[Any]] = ([(([(["a": 1, "n": 1, "d": 2] as! Any), (["a": 1, "n": 1, "d": 3] as! Any)] as! [Any]) as! Any), (([(["a": 2, "n": 1, "d": 3] as! Any), (["a": 1, "n": 1, "d": 7] as! Any)] as! [Any]) as! Any), (([(["a": 4, "n": 1, "d": 5] as! Any), (["a": -1, "n": 1, "d": 239] as! Any)] as! [Any]) as! Any), (([(["a": 5, "n": 1, "d": 7] as! Any), (["a": 2, "n": 3, "d": 79] as! Any)] as! [Any]) as! Any), (([(["a": 1, "n": 1, "d": 2] as! Any), (["a": 1, "n": 1, "d": 5] as! Any), (["a": 1, "n": 1, "d": 8] as! Any)] as! [Any]) as! Any), (([(["a": 4, "n": 1, "d": 5] as! Any), (["a": -1, "n": 1, "d": 70] as! Any), (["a": 1, "n": 1, "d": 99] as! Any)] as! [Any]) as! Any), (([(["a": 5, "n": 1, "d": 7] as! Any), (["a": 4, "n": 1, "d": 53] as! Any), (["a": 2, "n": 1, "d": 4443] as! Any)] as! [Any]) as! Any), (([(["a": 6, "n": 1, "d": 8] as! Any), (["a": 2, "n": 1, "d": 57] as! Any), (["a": 1, "n": 1, "d": 239] as! Any)] as! [Any]) as! Any), (([(["a": 8, "n": 1, "d": 10] as! Any), (["a": -1, "n": 1, "d": 239] as! Any), (["a": -4, "n": 1, "d": 515] as! Any)] as! [Any]) as! Any), (([(["a": 12, "n": 1, "d": 18] as! Any), (["a": 8, "n": 1, "d": 57] as! Any), (["a": -5, "n": 1, "d": 239] as! Any)] as! [Any]) as! Any), (([(["a": 16, "n": 1, "d": 21] as! Any), (["a": 3, "n": 1, "d": 239] as! Any), (["a": 4, "n": 3, "d": 1042] as! Any)] as! [Any]) as! Any), (([(["a": 22, "n": 1, "d": 28] as! Any), (["a": 2, "n": 1, "d": 443] as! Any), (["a": -5, "n": 1, "d": 1393] as! Any), (["a": -10, "n": 1, "d": 11018] as! Any)] as! [Any]) as! Any), (([(["a": 22, "n": 1, "d": 38] as! Any), (["a": 17, "n": 7, "d": 601] as! Any), (["a": 10, "n": 7, "d": 8149] as! Any)] as! [Any]) as! Any), (([(["a": 44, "n": 1, "d": 57] as! Any), (["a": 7, "n": 1, "d": 239] as! Any), (["a": -12, "n": 1, "d": 682] as! Any), (["a": 24, "n": 1, "d": 12943] as! Any)] as! [Any]) as! Any), (([(["a": 88, "n": 1, "d": 172] as! Any), (["a": 51, "n": 1, "d": 239] as! Any), (["a": 32, "n": 1, "d": 682] as! Any), (["a": 44, "n": 1, "d": 5357] as! Any), (["a": 68, "n": 1, "d": 12943] as! Any)] as! [Any]) as! Any), (([(["a": 88, "n": 1, "d": 172] as! Any), (["a": 51, "n": 1, "d": 239] as! Any), (["a": 32, "n": 1, "d": 682] as! Any), (["a": 44, "n": 1, "d": 5357] as! Any), (["a": 68, "n": 1, "d": 12944] as! Any)] as! [Any]) as! Any)] as! [[Any]])
    func format(_ ts: [[String: Int]]) -> String {
        var s: String = "["
        var i: Int = 0
        while (i < Int(((ts).count))) {
            let t: [String: Int] = (ts[i] as! [String: Int])
            s = ((((((((s + "{") + _p(t["a"]!)) + " ") + _p(t["n"]!)) + " ") + _p(t["d"]!)) + "}") as! String)
            if (i < (Int(((ts).count)) - 1)) {
                s = ((s + " ") as! String)
            }
            i = ((i + 1) as! Int)
        }
        return ((s + "]") as! String)
    }
    func tanEval(_ coef: Int, _ f: Double) -> Double {
        if (coef == 1) {
            return (f as! Double)
        }
        if (coef < 0) {
            return Double(-Double(tanEval((-coef as! Int), (f as! Double))))
        }
        let ca = (coef / 2)
        let cb = (coef - ca)
        let a = Double(tanEval((ca as! Int), (f as! Double)))
        let b = Double(tanEval((cb as! Int), (f as! Double)))
        return Double((Double((a + b)) / Double((1 - (a * b)))))
    }
    func tans(_ m: [[String: Int]]) -> Double {
        if (Int(((m).count)) == 1) {
            let t: [String: Int] = (m[0] as! [String: Int])
            return Double(tanEval((t["a"]! as! Int), Double(br((t["n"]! as! Int), (t["d"]! as! Int)))))
        }
        let half = (Int(((m).count)) / 2)
        let a = Double(tans((Array(m[0..<half]) as! [[String: Int]])))
        let b = Double(tans((Array(m[half..<m.count]) as! [[String: Int]])))
        return Double((Double((a + b)) / Double((1 - (a * b)))))
    }
    for ts in testCases {
        print(_p(((("tan " + String(describing: format((ts as! [[String: Int]])))) + " = ") + _p(Double(tans((ts as! [[String: Int]])))))))
    }
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
