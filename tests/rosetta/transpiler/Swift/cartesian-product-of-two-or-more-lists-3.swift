// Generated by Mochi transpiler v0.10.47 on 2025-07-28 12:12:08 GMT+7
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
    func listStr(_ xs: [Int]) -> String {
        var s: String = "["
        var i: Int = 0
        while (i < Int(((xs).count))) {
            s = ((s + _p((xs[i] as! Int))) as! String)
            if (i < (Int(((xs).count)) - 1)) {
                s = ((s + " ") as! String)
            }
            i = ((i + 1) as! Int)
        }
        s = ((s + "]") as! String)
        return (s as! String)
    }
    func llStr(_ lst: [[Int]]) -> String {
        var s: String = "["
        var i: Int = 0
        while (i < Int(((lst).count))) {
            s = ((s + String(describing: listStr((lst[i] as! [Int])))) as! String)
            if (i < (Int(((lst).count)) - 1)) {
                s = ((s + " ") as! String)
            }
            i = ((i + 1) as! Int)
        }
        s = ((s + "]") as! String)
        return (s as! String)
    }
    func concat(_ a: [Int], _ b: [Int]) -> [Int] {
        var out: [Int] = ([] as! [Int])
        for v in a {
            out = (_append(out, v) as! [Int])
        }
        for v in b {
            out = (_append(out, v) as! [Int])
        }
        return (out as! [Int])
    }
    func cartN(_ lists: Any?) -> [[Int]] {
        if (String(describing: lists)
        == String(describing: nil as Any?)
        ) {
            return ([] as! [[Int]])
        }
        let a: [[Int]] = (lists as! [[Int]])
        if (Int(((a).count)) == 0) {
            return (([[]] as! [[Any]]) as! [[Int]])
        }
        var out: [[Int]] = ([] as! [[Int]])
        let rest: [[Int]] = (cartN((Array(a[1..<Int(((a).count))]) as! [[Int]])) as! [[Int]])
        for x in (a[0] as! [Int]) {
            for p in rest {
                out = (_append(out, (concat(([x] as! [Int]), (p as! [Int])) as! [Int])) as! [[Int]])
            }
        }
        return (out as! [[Int]])
    }
    func main() {
        print(_p(String(describing: llStr((cartN(([([1, 2] as! [Int]), ([3, 4] as! [Int])] as! [[Int]])) as! [[Int]])))))
        print(_p(String(describing: llStr((cartN(([([3, 4] as! [Int]), ([1, 2] as! [Int])] as! [[Int]])) as! [[Int]])))))
        print(_p(String(describing: llStr((cartN([([1, 2] as! [Int]), []]) as! [[Int]])))))
        print(_p(String(describing: llStr((cartN([[], ([1, 2] as! [Int])]) as! [[Int]])))))
        print(_p(""))
        print(_p("["))
        for p in (cartN(([([1776, 1789] as! [Int]), ([7, 12] as! [Int]), ([4, 14, 23] as! [Int]), ([0, 1] as! [Int])] as! [[Int]])) as! [[Int]]) {
            print(_p((" " + String(describing: listStr((p as! [Int]))))))
        }
        print(_p("]"))
        print(_p(String(describing: llStr((cartN(([([1, 2, 3] as! [Int]), ([30] as! [Int]), ([500, 100] as! [Int])] as! [[Int]])) as! [[Int]])))))
        print(_p(String(describing: llStr((cartN([([1, 2, 3] as! [Int]), [], ([500, 100] as! [Int])]) as! [[Int]])))))
        print(_p(""))
        print(_p(String(describing: llStr((cartN(nil) as! [[Int]])))))
        print(_p(String(describing: llStr((cartN([]) as! [[Int]])))))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
