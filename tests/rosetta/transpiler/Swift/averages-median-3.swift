// Generated by Mochi transpiler v0.10.40 on 2025-07-26 10:33:10 GMT+7
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
    func qsel(_ a: [Double], _ k: Int) -> Double {
        var k = k
        var arr = a
        while (Int(((arr).count)) > 1) {
            var px = (Int(_now()) % Int(((arr).count)))
            var pv = arr[px]
            let last = (Int(((arr).count)) - 1)
            let tmp = arr[px]
            arr[px] = arr[last]
            arr[last] = Double(tmp)
            px = 0
            var i = 0
            while (i < last) {
                let v = arr[i]
                if (v < pv) {
                    let tmp2 = arr[px]
                    arr[px] = arr[i]
                    arr[i] = Double(tmp2)
                    px = Int((px + 1))
                }
                i = Int((i + 1))
            }
            if (px == k) {
                return Double(pv)
            }
            if (k < px) {
                arr = (Array(arr[0..<px]) as! [Double])
            } else {
                let tmp2 = arr[px]
                arr[px] = Double(pv)
                arr[last] = Double(tmp2)
                arr = (Array(arr[Int((px + 1))..<arr.count]) as! [Double])
                k = Int((k - Int((px + 1))))
            }
        }
        return (arr[0] as! Double)
    }
    func median(_ list: [Double]) -> Double {
        var arr = list
        let half = Int((Int(((arr).count)) / 2))
        let med = Double(qsel((arr as! [Double]), Int(half)))
        if ((Int(((arr).count)) % 2) == 0) {
            return Double((Double((med + Double(qsel((arr as! [Double]), Int((half - 1)))))) / 2.0))
        }
        return Double(med)
    }
    print(_p(_p(Double(median(([3.0, 1.0, 4.0, 1.0] as! [Double]))))))
    print(_p(_p(Double(median(([3.0, 1.0, 4.0, 1.0, 5.0] as! [Double]))))))
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
