// Generated by Mochi transpiler v0.10.40 on 2025-07-26 10:27:53 GMT+7
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
    func randOrder(_ seed: Int, _ n: Int) -> [Int] {
        let next = (Int(((seed * 1664525) + 1013904223)) % 2147483647)
        return ([next, (next % n)] as! [Int])
    }
    func randChaos(_ seed: Int, _ n: Int) -> [Int] {
        let next = (Int(((seed * 1103515245) + 12345)) % 2147483647)
        return ([next, (next % n)] as! [Int])
    }
    func main() {
        let nBuckets = 10
        let initialSum = 1000
        var buckets: [Int] = ([] as! [Int])
        for i in 0..<nBuckets {
            buckets = ((buckets + [0]) as! [Int])
        }
        var i = nBuckets
        var dist = initialSum
        while (i > 0) {
            let v = (dist / i)
            i = Int((i - 1))
            buckets[i] = Int(v)
            dist = Int((dist - v))
        }
        var tc0 = 0
        var tc1 = 0
        var total = 0
        var nTicks = 0
        var seedOrder = 1
        var seedChaos = 2
        print(_p("sum  ---updates---    mean  buckets"))
        var t = 0
        while (t < 5) {
            var r = (randOrder(Int(seedOrder), Int(nBuckets)) as! [Int])
            seedOrder = Int(r[0])
            var b1 = r[1]
            var b2 = (Int((b1 + 1)) % nBuckets)
            let v1 = buckets[b1]
            let v2 = buckets[b2]
            if (v1 > v2) {
                var a = Int((Int((v1 - v2)) / 2))
                if (a > buckets[b1]) {
                    a = Int(buckets[b1])
                }
                buckets[b1] = Int((buckets[b1] - a))
                buckets[b2] = Int((buckets[b2] + a))
            } else {
                var a = Int((Int((v2 - v1)) / 2))
                if (a > buckets[b2]) {
                    a = Int(buckets[b2])
                }
                buckets[b2] = Int((buckets[b2] - a))
                buckets[b1] = Int((buckets[b1] + a))
            }
            tc0 = Int((tc0 + 1))
            r = (randChaos(Int(seedChaos), Int(nBuckets)) as! [Int])
            seedChaos = Int(r[0])
            b1 = Int(r[1])
            b2 = Int((Int((b1 + 1)) % nBuckets))
            r = (randChaos(Int(seedChaos), Int((buckets[b1] + 1))) as! [Int])
            seedChaos = Int(r[0])
            var amt = r[1]
            if (amt > buckets[b1]) {
                amt = Int(buckets[b1])
            }
            buckets[b1] = Int((buckets[b1] - amt))
            buckets[b2] = Int((buckets[b2] + amt))
            tc1 = Int((tc1 + 1))
            var sum = 0
            var idx = 0
            while (idx < nBuckets) {
                sum = Int((sum + buckets[idx]))
                idx = Int((idx + 1))
            }
            total = Int(((total + tc0) + tc1))
            nTicks = Int((nTicks + 1))
            print(_p(((((((((_p(sum) + " ") + _p(tc0)) + " ") + _p(tc1)) + " ") + _p((total / nTicks))) + "  ") + _p(buckets))))
            tc0 = 0
            tc1 = 0
            t = Int((t + 1))
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
