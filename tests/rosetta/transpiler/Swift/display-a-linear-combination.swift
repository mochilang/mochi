// Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:32:41 GMT+7
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
    func padRight(_ s: String, _ w: Int) -> String {
        var r: String = s
        while (Int(((r).count)) < w) {
            r = String(describing: (r + " "))
        }
        return String(describing: r)
    }
    func linearCombo(_ c: [Int]) -> String {
        var out: String = ""
        var i: Int = 0
        while (i < Int(((c).count))) {
            let n = (c[i] as! Int)
            if (n != 0) {
                var op: String = ""
                if ((n < 0) && (Int(((out).count)) == 0)) {
                    op = "-"
                } else if (n < 0) {
                    op = " - "
                } else if ((n > 0) && (Int(((out).count)) == 0)) {
                    op = ""
                } else {
                    op = " + "
                }
                
                
                var av: Int = n
                if (av < 0) {
                    av = Int(-av)
                }
                var coeff: String = (_p(av) + "*")
                if (av == 1) {
                    coeff = ""
                }
                out = String(describing: (((((out + op) + coeff) + "e(") + _p((i + 1))) + ")"))
            }
            i = Int((i + 1))
        }
        if (Int(((out).count)) == 0) {
            return "0"
        }
        return String(describing: out)
    }
    func main() {
        let combos = ([([1, 2, 3] as! [Int]), ([0, 1, 2, 3] as! [Int]), ([1, 0, 3, 4] as! [Int]), ([1, 2, 0] as! [Int]), ([0, 0, 0] as! [Int]), ([0] as! [Int]), ([1, 1, 1] as! [Int]), ([-1, -1, -1] as! [Int]), ([-1, -2, 0, -3] as! [Int]), ([-1] as! [Int])] as! [[Int]])
        var idx: Int = 0
        while (idx < Int(((combos).count))) {
            let c = (combos[idx] as! [Int])
            var t: String = "["
            var j: Int = 0
            while (j < Int(((c).count))) {
                t = String(describing: (t + _p((c[j] as! Int))))
                if (j < (Int(((c).count)) - 1)) {
                    t = String(describing: (t + ", "))
                }
                j = Int((j + 1))
            }
            t = String(describing: (t + "]"))
            let lc = String(describing: linearCombo((c as! [Int])))
            print(_p(((String(describing: padRight(String(describing: t), 15)) + "  ->  ") + lc)))
            idx = Int((idx + 1))
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
