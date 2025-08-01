// Generated by Mochi transpiler v0.10.50 on 2025-07-31 00:24:23 GMT+7
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
    func findDuplicates(_ fs: [String: String], _ paths: [String]) -> [[String]] {
        var seen: [String: String] = ([:] as! [String: String])
        var dups: [[String]] = ([] as! [[String]])
        for path in paths {
            let content = fs[path]!
            if (seen[content] != nil) {
                dups = (_append(dups, ([seen[content]!, path] as! [String])) as! [[String]])
            } else {
                seen[content] = (path as! String)
            }
        }
        return (dups as! [[String]])
    }
    func main() {
        var fs: [String: String] = (["a.txt": "hello", "b.txt": "world", "c.txt": "hello", "d.txt": "foo", "e.txt": "world"] as! [String: String])
        let paths: [String] = (["a.txt", "b.txt", "c.txt", "d.txt", "e.txt"] as! [String])
        let dups: [[String]] = (findDuplicates((fs as! [String: String]), (paths as! [String])) as! [[String]])
        for pair in dups {
            print(_p((((pair[0] as! String) + " <==> ") + (pair[1] as! String))))
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
