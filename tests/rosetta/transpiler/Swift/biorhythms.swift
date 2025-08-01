// Generated by Mochi transpiler v0.10.41 on 2025-07-27 16:25:59 GMT+7
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
    let PI: Double = 3.141592653589793
    let TWO_PI: Double = 6.283185307179586
    func sinApprox(_ x: Double) -> Double {
        var term: Double = x
        var sum: Double = x
        var n: Int = 1
        while (n <= 8) {
            let denom = Double((Int((2 * n)) * Int(((2 * n) + 1))))
            term = Double((((-term * x) * x) / denom))
            sum = Double((sum + term))
            n = Int((n + 1))
        }
        return Double(sum)
    }
    func floor(_ x: Double) -> Double {
        var i: Int = Int(x)
        if (Double(i) > x) {
            i = Int((i - 1))
        }
        return Double(i)
    }
    func absFloat(_ x: Double) -> Double {
        if (x < 0.0) {
            return Double(-x)
        }
        return Double(x)
    }
    func absInt(_ n: Int) -> Int {
        if (n < 0) {
            return Int(-n)
        }
        return Int(n)
    }
    func parseIntStr(_ str: String) -> Int {
        var i: Int = 0
        var neg: Bool = false
        if ((Int(((str).count)) > 0) && (String(Array(str)[0..<1]) == "-")) {
            neg = true
            i = 1
        }
        var n: Int = 0
        let digits: [String: Any] = ["0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9]
        while (i < Int(((str).count))) {
            n = Int(((n * 10) + ((digits as! [String: Any])[String(Array(str)[i..<(i + 1)])] as! Int)))
            i = Int((i + 1))
        }
        if neg {
            n = Int(-n)
        }
        return Int(n)
    }
    func parseDate(_ s: String) -> [Int] {
        let y = Int(Int(String(describing: String(describing: String(Array(s)[0..<4]))))!)
        let m = Int(Int(String(describing: String(describing: String(Array(s)[5..<7]))))!)
        let d = Int(Int(String(describing: String(describing: String(Array(s)[8..<10]))))!)
        return ([y, m, d] as! [Int])
    }
    func leap(_ y: Int) -> Bool {
        if ((y % 400) == 0) {
            return true
        }
        if ((y % 100) == 0) {
            return false
        }
        return (((y % 4) == 0) as! Bool)
    }
    func daysInMonth(_ y: Int, _ m: Int) -> Int {
        let feb = Int(((leap(Int(y)) as! Bool) ? 29 : 28))
        let lengths = ([31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] as! [Int])
        return (lengths[(m - 1)] as! Int)
    }
    func addDays(_ y: Int, _ m: Int, _ d: Int, _ n: Int) -> [Int] {
        var yy: Int = y
        var mm: Int = m
        var dd: Int = d
        if (n >= 0) {
            var i: Int = 0
            while (i < n) {
                dd = Int((dd + 1))
                if (dd > Int(daysInMonth(Int(yy), Int(mm)))) {
                    dd = 1
                    mm = Int((mm + 1))
                    if (mm > 12) {
                        mm = 1
                        yy = Int((yy + 1))
                    }
                }
                i = Int((i + 1))
            }
        } else {
            var i: Int = 0
            while (i > n) {
                dd = Int((dd - 1))
                if (dd < 1) {
                    mm = Int((mm - 1))
                    if (mm < 1) {
                        mm = 12
                        yy = Int((yy - 1))
                    }
                    dd = Int(daysInMonth(Int(yy), Int(mm)))
                }
                i = Int((i - 1))
            }
        }
        return ([yy, mm, dd] as! [Int])
    }
    func pad2(_ n: Int) -> String {
        if (n < 10) {
            return String(describing: ("0" + _p(n)))
        }
        return String(describing: _p(n))
    }
    func dateString(_ y: Int, _ m: Int, _ d: Int) -> String {
        return String(describing: ((((_p(y) + "-") + String(describing: pad2(Int(m)))) + "-") + String(describing: pad2(Int(d)))))
    }
    func day(_ y: Int, _ m: Int, _ d: Int) -> Int {
        let part1 = (367 * y)
        let part2 = Int((Int((7 * Int((y + Int((Int((m + 9)) / 12)))))) / 4))
        let part3 = Int((Int((275 * m)) / 9))
        return Int(((((part1 - part2) + part3) + d) - 730530))
    }
    func biorhythms(_ birth: String, _ target: String) {
        let bparts = (parseDate(String(describing: birth)) as! [Int])
        let by = (bparts[0] as! Int)
        let bm = (bparts[1] as! Int)
        let bd = (bparts[2] as! Int)
        let tparts = (parseDate(String(describing: target)) as! [Int])
        let ty = (tparts[0] as! Int)
        let tm = (tparts[1] as! Int)
        let td = (tparts[2] as! Int)
        let diff = Int(absInt(Int((Int(day(Int(ty), Int(tm), Int(td))) - Int(day(Int(by), Int(bm), Int(bd)))))))
        print(_p(((("Born " + birth) + ", Target ") + target)))
        print(_p(("Day " + _p(diff))))
        let cycles = (["Physical day ", "Emotional day", "Mental day   "] as! [String])
        let lengths = ([23, 28, 33] as! [Int])
        let quadrants = ([(["up and rising", "peak"] as! [String]), (["up but falling", "transition"] as! [String]), (["down and falling", "valley"] as! [String]), (["down but rising", "transition"] as! [String])] as! [[String]])
        var i: Int = 0
        while (i < 3) {
            let length = (lengths[i] as! Int)
            let cycle = String(describing: cycles[i])
            let position = (diff % length)
            let quadrant = (Int((position * 4)) / length)
            var percent: Double = Double(sinApprox(Double((((2.0 * PI) * Double(position)) / Double(length)))))
            percent = Double((Double(floor(Double((percent * 1000.0)))) / 10.0))
            var description: String = ""
            if (percent > 95.0) {
                description = " peak"
            } else if (percent < Double(-95.0)) {
                description = " valley"
            } else if (Double(absFloat(Double(percent))) < 5.0) {
                description = " critical transition"
            } else {
                let daysToAdd = (((Int((quadrant + 1)) * length) / 4) - position)
                let res = (addDays(Int(ty), Int(tm), Int(td), Int(daysToAdd)) as! [Int])
                let ny = (res[0] as! Int)
                let nm = (res[1] as! Int)
                let nd = (res[2] as! Int)
                let transition = String(describing: dateString(Int(ny), Int(nm), Int(nd)))
                let trend = String(describing: quadrants[quadrant][0])
                let next = String(describing: quadrants[quadrant][1])
                var pct: String = _p(percent)
                if (!((String(describing: pct).contains(".")) as! Bool)) {
                    pct = String(describing: (pct + ".0"))
                }
                description = String(describing: ((((((((" " + pct) + "% (") + trend) + ", next ") + next) + " ") + transition) + ")"))
            }
            
            
            var posStr: String = _p(position)
            if (position < 10) {
                posStr = String(describing: (" " + posStr))
            }
            print(_p((((cycle + posStr) + " : ") + description)))
            i = Int((i + 1))
        }
        print(_p(""))
    }
    func main() {
        let pairs = ([(["1943-03-09", "1972-07-11"] as! [String]), (["1809-01-12", "1863-11-19"] as! [String]), (["1809-02-12", "1863-11-19"] as! [String])] as! [[String]])
        var idx: Int = 0
        while (idx < Int(((pairs).count))) {
            let p = (pairs[idx] as! [String])
            _ = biorhythms(String(describing: p[0]), String(describing: p[1]))
            idx = Int((idx + 1))
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
