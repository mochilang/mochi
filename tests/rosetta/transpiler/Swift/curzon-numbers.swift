// Generated by Mochi transpiler v0.10.52 on 2025-08-02 01:28:34 GMT+7
import Foundation

func _p(_ v: Any?) -> String {
    if let val = v {
        if let d = val as? Double {
            if d.rounded(.towardZero) == d {
                return String(Int64(d))
            }
        }
        return String(describing: val)
    }
    return "<nil>"
}

func _append<T>(_ xs: [T], _ v: T) -> [T] {
    var out = xs
    out.append(v)
    return out
}
func padLeft(_ n: Int, _ width: Int) -> String {
    var s: String = _p(n)
    while (Int(((s).count)) < width) {
        s = ((" " + s) as! String)
    }
    return s
}
func modPow(_ base: Int, _ exp: Int, _ mod: Int) -> Int {
    var result: Int = (1 % mod)
    var b: Int = (base % mod)
    var e: Int = exp
    while (e > 0) {
        if ((e % 2) == 1) {
            result = ((Int((result &* b)) % mod) as! Int)
        }
        b = ((Int((b &* b)) % mod) as! Int)
        e = ((e / 2) as! Int)
    }
    return result
}
func main() {
    var k: Int = 2
    while (k <= 10) {
        print(_p((("The first 50 Curzon numbers using a base of " + _p(k)) + " :")))
        var count: Int = 0
        var n: Int = 1
        var curzon50: [Int] = ([] as! [Int])
        while true {
            let d = ((k &* n) &+ 1)
            if ((Int((Int(modPow((k as! Int), (n as! Int), (d as! Int))) &+ 1)) % d) == 0) {
                if (count < 50) {
                    curzon50 = (_append(curzon50, n) as! [Int])
                }
                count = ((count &+ 1) as! Int)
                if (count == 50) {
                    var idx: Int = 0
                    while (idx < Int(((curzon50).count))) {
                        var line: String = ""
                        var j: Int = 0
                        while (j < 10) {
                            line = (((line + String(describing: padLeft((curzon50[idx] as! Int), 4))) + " ") as! String)
                            idx = ((idx &+ 1) as! Int)
                            j = ((j &+ 1) as! Int)
                        }
                        print(_p(String(describing: String(Array(String(describing: (line as! String)))[0..<((Int(((line).count)) &- 1) as! Int)]))))
                    }
                }
                if (count == 1000) {
                    print(_p(("\nOne thousandth: " + _p(n))))
                    break
                }
            }
            n = ((n &+ 1) as! Int)
        }
        print(_p(""))
        k = ((k &+ 2) as! Int)
    }
}
_ = main()
