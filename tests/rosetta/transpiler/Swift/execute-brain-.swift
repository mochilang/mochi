// Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:42:43 GMT+7
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
    func chr(_ n: Int) -> String {
        let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower = "abcdefghijklmnopqrstuvwxyz"
        if ((n >= 65) && (n < 91)) {
            return String(describing: String(Array(upper)[(n - 65)..<(n - 64)]))
        }
        if ((n >= 97) && (n < 123)) {
            return String(describing: String(Array(lower)[(n - 97)..<(n - 96)]))
        }
        if (n == 32) {
            return " "
        }
        if (n == 33) {
            return "!"
        }
        if (n == 44) {
            return ","
        }
        if (n == 13) {
            return ""
        }
        if (n == 10) {
            return "\n"
        }
        return "?"
    }
    func bf(_ dLen: Int, _ code: String) -> String {
        var ds: [Int] = ([] as! [Int])
        for i in 0..<dLen {
            ds = (_append(ds, 0) as! [Int])
        }
        var dp: Int = 0
        var ip: Int = 0
        var out: String = ""
        while (ip < Int(((code).count))) {
            let ch = String(Array(code)[ip..<(ip + 1)])
            if (ch == ">") {
                dp = Int((dp + 1))
            } else if (ch == "<") {
                dp = Int((dp - 1))
            } else if (ch == "+") {
                ds[dp] = Int(((ds[dp] as! Int) + 1))
            } else if (ch == "-") {
                ds[dp] = Int(((ds[dp] as! Int) - 1))
            } else if (ch == ".") {
                out = String(describing: (out + String(describing: chr((ds[dp] as! Int)))))
            } else if (ch == ",") {
            } else if (ch == "[") {
                if ((ds[dp] as! Int) == 0) {
                    var nc: Int = 1
                    while (nc > 0) {
                        ip = Int((ip + 1))
                        let cc = String(Array(code)[ip..<(ip + 1)])
                        if (cc == "[") {
                            nc = Int((nc + 1))
                        } else if (cc == "]") {
                            nc = Int((nc - 1))
                        }
                        
                    }
                }
            } else if (ch == "]") {
                if ((ds[dp] as! Int) != 0) {
                    var nc: Int = 1
                    while (nc > 0) {
                        ip = Int((ip - 1))
                        let cc = String(Array(code)[ip..<(ip + 1)])
                        if (cc == "]") {
                            nc = Int((nc + 1))
                        } else if (cc == "[") {
                            nc = Int((nc - 1))
                        }
                        
                    }
                }
            }
            
            
            
            
            
            
            
            ip = Int((ip + 1))
        }
        return String(describing: out)
    }
    func main() {
        let prog = ((("++++++++++[>+>+++>++++>+++++++>++++++++>+++++++++>++\n" + "++++++++>+++++++++++>++++++++++++<<<<<<<<<-]>>>>+.>>>\n") + ">+..<.<++++++++.>>>+.<<+.<<<<++++.<++.>>>+++++++.>>>.+++.\n") + "<+++++++.--------.<<<<<+.<+++.---.")
        let out = String(describing: bf(10, String(describing: prog)))
        print(_p(out))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
