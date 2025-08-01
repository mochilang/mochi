// Generated by Mochi transpiler v0.10.41 on 2025-07-27 01:19:44 GMT+7
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
    func sameDigits(_ n: Int, _ b: Int) -> Bool {
        var n = n
        var f: Int = (n % b)
        n = Int((n / b))
        while (n > 0) {
            if ((n % b) != f) {
                return false
            }
            n = Int((n / b))
        }
        return true
    }
    func isBrazilian(_ n: Int) -> Bool {
        if (n < 7) {
            return false
        }
        if (((n % 2) == 0) && (n >= 8)) {
            return true
        }
        var b: Int = 2
        while (b < (n - 1)) {
            if (sameDigits(Int(n), Int(b)) as! Bool) {
                return true
            }
            b = Int((b + 1))
        }
        return false
    }
    func isPrime(_ n: Int) -> Bool {
        if (n < 2) {
            return false
        }
        if ((n % 2) == 0) {
            return ((n == 2) as! Bool)
        }
        if ((n % 3) == 0) {
            return ((n == 3) as! Bool)
        }
        var d: Int = 5
        while ((d * d) <= n) {
            if ((n % d) == 0) {
                return false
            }
            d = Int((d + 2))
            if ((n % d) == 0) {
                return false
            }
            d = Int((d + 4))
        }
        return true
    }
    func main() {
        var kinds: [String] = ([" ", " odd ", " prime "] as! [String])
        for kind in kinds {
            print(_p((("First 20" + kind) + "Brazilian numbers:")))
            var c: Int = 0
            var n: Int = 7
            while true {
                if (isBrazilian(Int(n)) as! Bool) {
                    print(_p((_p(n) + " ")))
                    c = Int((c + 1))
                    if (c == 20) {
                        print(_p("\n"))
                        break
                    }
                }
                if (kind == " ") {
                    n = Int((n + 1))
                } else if (kind == " odd ") {
                    n = Int((n + 2))
                } else {
                    while true {
                        n = Int((n + 2))
                        if (isPrime(Int(n)) as! Bool) {
                            break
                        }
                    }
                }
                
            }
        }
        var n: Int = 7
        var c: Int = 0
        while (c < 100000) {
            if (isBrazilian(Int(n)) as! Bool) {
                c = Int((c + 1))
            }
            n = Int((n + 1))
        }
        print(_p(("The 100,000th Brazilian number: " + _p((n - 1)))))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
