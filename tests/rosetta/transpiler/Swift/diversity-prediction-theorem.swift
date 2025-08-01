// Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:32:49 GMT+7
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
    func pow10(_ n: Int) -> Double {
        var r: Double = 1.0
        var i: Int = 0
        while (i < n) {
            r = Double((r * 10.0))
            i = Int((i + 1))
        }
        return Double(r)
    }
    func formatFloat(_ f: Double, _ prec: Int) -> String {
        let scale = Double(pow10(Int(prec)))
        let scaled = (Double((f * scale)) + 0.5)
        var n: Int = Int(scaled)
        var digits: String = _p(n)
        while (Int(((digits).count)) <= prec) {
            digits = String(describing: ("0" + digits))
        }
        let intPart = String(describing: String(Array(String(describing: String(describing: digits)))[0..<Int((Int(((digits).count)) - prec))]))
        let fracPart = String(describing: String(Array(String(describing: String(describing: digits)))[Int((Int(((digits).count)) - prec))..<Int(((digits).count))]))
        return String(describing: ((intPart + ".") + fracPart))
    }
    func padLeft(_ s: String, _ w: Int) -> String {
        var res: String = ""
        var n: Int = (w - Int(((s).count)))
        while (n > 0) {
            res = String(describing: (res + " "))
            n = Int((n - 1))
        }
        return String(describing: (res + s))
    }
    func averageSquareDiff(_ f: Double, _ preds: [Double]) -> Double {
        var av: Double = 0.0
        var i: Int = 0
        while (i < Int(((preds).count))) {
            av = Double((av + (Double((Double(preds[i]) - f)) * Double((Double(preds[i]) - f)))))
            i = Int((i + 1))
        }
        av = Double((av / Double(((preds).count))))
        return Double(av)
    }
    func diversityTheorem(_ truth: Double, _ preds: [Double]) -> [Double] {
        var av: Double = 0.0
        var i: Int = 0
        while (i < Int(((preds).count))) {
            av = Double((av + Double(preds[i])))
            i = Int((i + 1))
        }
        av = Double((av / Double(((preds).count))))
        let avErr = Double(averageSquareDiff(Double(truth), (preds as! [Double])))
        let crowdErr = (Double((truth - av)) * Double((truth - av)))
        let div = Double(averageSquareDiff(Double(av), (preds as! [Double])))
        return ([avErr, crowdErr, div] as! [Double])
    }
    func main() {
        let predsArray = ([([48.0, 47.0, 51.0] as! [Double]), ([48.0, 47.0, 51.0, 42.0] as! [Double])] as! [[Double]])
        let truth = 49.0
        var i: Int = 0
        while (i < Int(((predsArray).count))) {
            let preds = (predsArray[i] as! [Double])
            let res = (diversityTheorem(Double(truth), (preds as! [Double])) as! [Double])
            print(_p(("Average-error : " + String(describing: padLeft(String(describing: formatFloat(Double(res[0]), 3)), 6)))))
            print(_p(("Crowd-error   : " + String(describing: padLeft(String(describing: formatFloat(Double(res[1]), 3)), 6)))))
            print(_p(("Diversity     : " + String(describing: padLeft(String(describing: formatFloat(Double(res[2]), 3)), 6)))))
            print(_p(""))
            i = Int((i + 1))
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
