// Generated by Mochi transpiler v0.10.41 on 2025-07-27 16:28:04 GMT+7
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
func _sha256(_ bs: [Int]) -> [Int] { return Array(repeating: 0, count: 32) }
do {
    let _benchMemStart = _mem()
    let _benchStart = _now()
    func indexOf(_ s: String, _ ch: String) -> Int {
        var i: Int = 0
        while (i < Int(((s).count))) {
            if (String(Array(s)[i..<(i + 1)]) == ch) {
                return Int(i)
            }
            i = Int((i + 1))
        }
        return Int(-1)
    }
    func set58(_ addr: String) -> [Int] {
        let tmpl = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
        var a: [Int] = ([] as! [Int])
        var i: Int = 0
        while (i < 25) {
            a = ((a + [0]) as! [Int])
            i = Int((i + 1))
        }
        var idx: Int = 0
        while (idx < Int(((addr).count))) {
            let ch = String(Array(addr)[idx..<(idx + 1)])
            var c: Int = Int((String(describing: String(describing: tmpl)).firstIndex(of: Character(String(describing: String(describing: ch))))?.utf16Offset(in: String(describing: String(describing: tmpl)) ) ?? -1))
            if (c < 0) {
                return ([] as! [Int])
            }
            var j: Int = 24
            while (j >= 0) {
                c = Int((c + (58 * (a[j] as! Int))))
                a[j] = Int((c % 256))
                c = Int((c / 256))
                j = Int((j - 1))
            }
            if (c > 0) {
                return ([] as! [Int])
            }
            idx = Int((idx + 1))
        }
        return (a as! [Int])
    }
    func doubleSHA256(_ bs: [Int]) -> [Int] {
        let first = (_sha256(bs) as! [Int])
        return (_sha256(first) as! [Int])
    }
    func computeChecksum(_ a: [Int]) -> [Int] {
        let hash = (doubleSHA256((Array(a[0..<21]) as! [Int])) as! [Int])
        return (Array(hash[0..<4]) as! [Int])
    }
    func validA58(_ addr: String) -> Bool {
        let a = (set58(String(describing: addr)) as! [Int])
        if (Int(((a).count)) != 25) {
            return false
        }
        if ((a[0] as! Int) != 0) {
            return false
        }
        let sum = (computeChecksum((a as! [Int])) as! [Int])
        var i: Int = 0
        while (i < 4) {
            if ((a[(21 + i)] as! Int) != (sum[i] as! Int)) {
                return false
            }
            i = Int((i + 1))
        }
        return true
    }
    print(_p(_p((validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i") as! Bool))))
    print(_p(_p((validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j") as! Bool))))
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
