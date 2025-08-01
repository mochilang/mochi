// Generated by Mochi transpiler v0.10.50 on 2025-07-31 00:24:04 GMT+7
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
    func log10floor(_ n: Int) -> Int {
        var p: Int = 0
        var v: Int = n
        while (v >= 10) {
            v = ((v / 10) as! Int)
            p = ((p + 1) as! Int)
        }
        return (p as! Int)
    }
    func commatize(_ n: Int) -> String {
        var s: String = _p(n)
        var res: String = ""
        var i: Int = 0
        while (i < Int(((s).count))) {
            if ((i > 0) && ((Int((Int(((s).count)) - i)) % 3) == 0)) {
                res = ((res + ",") as! String)
            }
            res = ((res + String(describing: String(Array(String(describing: (s as! String)))[(i as! Int)..<((i + 1) as! Int)]))) as! String)
            i = ((i + 1) as! Int)
        }
        return (res as! String)
    }
    func showDistribution(_ sizes: [Int]) {
        var bins: [Int] = ([] as! [Int])
        var i: Int = 0
        while (i < 12) {
            bins = (_append(bins, 0) as! [Int])
            i = ((i + 1) as! Int)
        }
        var total: Int = 0
        for sz in sizes {
            total = ((total + sz) as! Int)
            var idx: Int = 0
            if (sz > 0) {
                idx = ((Int(log10floor((sz as! Int))) + 1) as! Int)
            }
            bins[idx] = (((bins[idx] as! Int) + 1) as! Int)
        }
        print(_p("File size distribution:\n"))
        i = 0
        while (i < Int(((bins).count))) {
            var prefix: String = "  "
            if (i > 0) {
                prefix = "+ "
            }
            print(_p(((((prefix + "Files less than 10 ^ ") + _p(i)) + " bytes : ") + _p((bins[i] as! Int)))))
            i = ((i + 1) as! Int)
        }
        print(_p("                                  -----"))
        print(_p(("= Total number of files         : " + _p(Int(((sizes).count))))))
        print(_p((("  Total size of files           : " + String(describing: commatize((total as! Int)))) + " bytes")))
    }
    func main() {
        let sizes: [Int] = ([0, 1, 9, 10, 99, 100, 1234, 50000, 730000, 8200000] as! [Int])
        _ = showDistribution((sizes as! [Int]))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
