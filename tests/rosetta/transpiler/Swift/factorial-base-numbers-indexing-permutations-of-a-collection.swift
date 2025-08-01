// Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:42:56 GMT+7
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
    func split(_ s: String, _ sep: String) -> [String] {
        var parts: [String] = ([] as! [String])
        var cur: String = ""
        var i: Int = 0
        while (i < Int(((s).count))) {
            if (((Int(((sep).count)) > 0) && ((i + Int(((sep).count))) <= Int(((s).count)))) && (String(describing: String(Array(String(describing: String(describing: s)))[Int(i)..<Int((i + Int(((sep).count))))])) == sep)) {
                parts = (_append(parts, cur) as! [String])
                cur = ""
                i = Int((i + Int(((sep).count))))
            } else {
                cur = String(describing: (cur + String(describing: String(Array(String(describing: String(describing: s)))[Int(i)..<Int((i + 1))]))))
                i = Int((i + 1))
            }
        }
        parts = (_append(parts, cur) as! [String])
        return (parts as! [String])
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
            n = Int(((n * 10) + ((digits as! [String: Any])[String(Array(str)[i..<(i + 1)])] as! Any)))
            i = Int((i + 1))
        }
        if neg {
            n = Int(-n)
        }
        return Int(n)
    }
    func joinInts(_ nums: [Int], _ sep: String) -> String {
        var s: String = ""
        var i: Int = 0
        while (i < Int(((nums).count))) {
            if (i > 0) {
                s = String(describing: (s + sep))
            }
            s = String(describing: (s + _p((nums[i] as! Int))))
            i = Int((i + 1))
        }
        return String(describing: s)
    }
    func undot(_ s: String) -> [Int] {
        let parts = (split(String(describing: s), ".") as! [String])
        var nums: [Int] = ([] as! [Int])
        for p in parts {
            nums = (_append(nums, Int(Int(String(describing: String(describing: p)))!)) as! [Int])
        }
        return (nums as! [Int])
    }
    func factorial(_ n: Int) -> Int {
        var f: Int = 1
        var i: Int = 2
        while (i <= n) {
            f = Int((f * i))
            i = Int((i + 1))
        }
        return Int(f)
    }
    func genFactBaseNums(_ size: Int, _ countOnly: Bool) -> [Any] {
        var results: [[Int]] = ([] as! [[Int]])
        var count: Int = 0
        var n: Int = 0
        while true {
            var radix: Int = 2
            var res: [Int] = ([] as! [Int])
            if (!countOnly) {
                var z: Int = 0
                while (z < size) {
                    res = (_append(res, 0) as! [Int])
                    z = Int((z + 1))
                }
            }
            var k: Int = n
            while (k > 0) {
                var div: Int = (k / radix)
                var rem: Int = (k % radix)
                if ((!countOnly) && (radix <= (size + 1))) {
                    res[((size - radix) + 1)] = Int(rem)
                }
                k = Int(div)
                radix = Int((radix + 1))
            }
            if (radix > (size + 2)) {
                break
            }
            count = Int((count + 1))
            if (!countOnly) {
                results = (_append(results, res) as! [[Int]])
            }
            n = Int((n + 1))
        }
        return ([results, count] as! [Any])
    }
    func mapToPerms(_ factNums: [[Int]]) -> [[Int]] {
        var perms: [[Int]] = ([] as! [[Int]])
        var psize: Int = (Int((((factNums[0] as! [Int])).count)) + 1)
        var start: [Int] = ([] as! [Int])
        var i: Int = 0
        while (i < psize) {
            start = (_append(start, i) as! [Int])
            i = Int((i + 1))
        }
        for fn in factNums {
            var perm: [Int] = ([] as! [Int])
            var j: Int = 0
            while (j < Int(((start).count))) {
                perm = (_append(perm, (start[j] as! Int)) as! [Int])
                j = Int((j + 1))
            }
            var m: Int = 0
            while (m < Int(((fn).count))) {
                var g: Int = (fn[m] as! Int)
                if (g != 0) {
                    var first: Int = m
                    var last: Int = (m + g)
                    var t: Int = 1
                    while (t <= g) {
                        var temp: Int = (perm[first] as! Int)
                        var x: Int = (first + 1)
                        while (x <= last) {
                            perm[(x - 1)] = (perm[x] as! Int)
                            x = Int((x + 1))
                        }
                        perm[last] = Int(temp)
                        t = Int((t + 1))
                    }
                }
                m = Int((m + 1))
            }
            perms = (_append(perms, perm) as! [[Int]])
        }
        return (perms as! [[Int]])
    }
    var seed: Int = 1
    func randInt(_ n: Int) -> Int {
        seed = Int((Int(((seed * 1664525) + 1013904223)) % 2147483647))
        return Int((seed % n))
    }
    func main() {
        let g = (genFactBaseNums(3, false) as! [Any])
        var factNums: Any = ((g as! [Any])[0] as! Any)
        var perms: [[Int]] = (mapToPerms((factNums as! [[Int]])) as! [[Int]])
        var i: Int = 0
        while (i < Int((String(describing: factNums).count))) {
            print(_p(((String(describing: joinInts((((factNums as! [Any])[i] as! Any) as! [Int]), ".")) + " -> ") + String(describing: joinInts((perms[i] as! [Int]), "")))))
            i = Int((i + 1))
        }
        let count2 = Int(factorial(11))
        print(_p(("\nPermutations generated = " + _p(count2))))
        print(_p(("compared to 11! which  = " + _p(Int(factorial(11))))))
        print(_p(""))
        let fbn51s: [String] = (["39.49.7.47.29.30.2.12.10.3.29.37.33.17.12.31.29.34.17.25.2.4.25.4.1.14.20.6.21.18.1.1.1.4.0.5.15.12.4.3.10.10.9.1.6.5.5.3.0.0.0", "51.48.16.22.3.0.19.34.29.1.36.30.12.32.12.29.30.26.14.21.8.12.1.3.10.4.7.17.6.21.8.12.15.15.13.15.7.3.12.11.9.5.5.6.6.3.4.0.3.2.1"] as! [String])
        factNums = ([(undot(String(describing: fbn51s[0])) as! [Int]), (undot(String(describing: fbn51s[1])) as! [Int])] as! [[Int]])
        perms = (mapToPerms((factNums as! [[Int]])) as! [[Int]])
        let shoe = "A♠K♠Q♠J♠T♠9♠8♠7♠6♠5♠4♠3♠2♠A♥K♥Q♥J♥T♥9♥8♥7♥6♥5♥4♥3♥2♥A♦K♦Q♦J♦T♦9♦8♦7♦6♦5♦4♦3♦2♦A♣K♣Q♣J♣T♣9♣8♣7♣6♣5♣4♣3♣2♣"
        var cards: [String] = ([] as! [String])
        i = 0
        while (i < 52) {
            var card: String = String(describing: String(Array(String(describing: String(describing: shoe)))[Int((2 * i))..<Int(((2 * i) + 2))]))
            if (String(Array(card)[0..<1]) == "T") {
                card = String(describing: ("10" + String(Array(card)[1..<2])))
            }
            cards = (_append(cards, card) as! [String])
            i = Int((i + 1))
        }
        i = 0
        while (i < Int(((fbn51s).count))) {
            print(_p(String(describing: fbn51s[i])))
            var perm: [Int] = (perms[i] as! [Int])
            var j: Int = 0
            var line: String = ""
            while (j < Int(((perm).count))) {
                line = String(describing: (line + String(describing: cards[(perm[j] as! Int)])))
                j = Int((j + 1))
            }
            print(_p((line + "\n")))
            i = Int((i + 1))
        }
        var fbn51: [Int] = ([] as! [Int])
        i = 0
        while (i < 51) {
            fbn51 = (_append(fbn51, Int(randInt(Int((52 - i))))) as! [Int])
            i = Int((i + 1))
        }
        print(_p(String(describing: joinInts((fbn51 as! [Int]), "."))))
        perms = (mapToPerms(([fbn51] as! [[Int]])) as! [[Int]])
        var line: String = ""
        i = 0
        while (i < Int((((perms[0] as! [Int])).count))) {
            line = String(describing: (line + String(describing: cards[(perms[0][i] as! Int)])))
            i = Int((i + 1))
        }
        print(_p(line))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
