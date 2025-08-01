// Generated by Mochi transpiler v0.10.41 on 2025-07-27 16:12:28 GMT+7
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
func _padStart(_ s: String, _ w: Int, _ p: String) -> String {
    var out = s
    while out.count < w { out = p + out }
    return out
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
struct BigInt: Comparable, CustomStringConvertible {
    private var digits: [UInt32] = []
    init() {}
    init(_ v: Int) {
        var n = v
        while n > 0 { digits.append(UInt32(n % 1000000000)); n /= 1000000000 }
    }
    init(_ other: BigInt) { digits = other.digits }
    func toInt() -> Int {
        var res: Int64 = 0
        for d in digits.reversed() { res = res * 1000000000 + Int64(d) }
        if res > Int64(Int.max) { return Int.max }
        return Int(res)
    }
    static func +(lhs: BigInt, rhs: BigInt) -> BigInt {
        var carry: UInt64 = 0
        var result: [UInt32] = []
        let maxLen = max(lhs.digits.count, rhs.digits.count)
        for i in 0..<maxLen {
            let a = i < lhs.digits.count ? UInt64(lhs.digits[i]) : 0
            let b = i < rhs.digits.count ? UInt64(rhs.digits[i]) : 0
            let sum = a + b + carry
            result.append(UInt32(sum % 1000000000))
            carry = sum / 1000000000
        }
        if carry > 0 { result.append(UInt32(carry)) }
        var r = BigInt(); r.digits = result; return r
    }
    static func -(lhs: BigInt, rhs: BigInt) -> BigInt {
        var borrow: Int64 = 0
        var result: [UInt32] = []
        for i in 0..<lhs.digits.count {
            var a = Int64(lhs.digits[i]) - borrow
            let b = i < rhs.digits.count ? Int64(rhs.digits[i]) : 0
            var diff = a - b
            if diff < 0 { diff += 1000000000; borrow = 1 } else { borrow = 0 }
            result.append(UInt32(diff))
        }
        while result.last == 0 { result.removeLast(); if result.isEmpty { break } }
        var r = BigInt(); r.digits = result; return r
    }
    static func *(lhs: BigInt, rhs: BigInt) -> BigInt {
        if lhs.digits.isEmpty || rhs.digits.isEmpty { return BigInt() }
        var result = Array(repeating: UInt64(0), count: lhs.digits.count + rhs.digits.count)
        for i in 0..<lhs.digits.count {
            var carry: UInt64 = 0
            for j in 0..<rhs.digits.count {
                let idx = i + j
                let prod = UInt64(lhs.digits[i]) * UInt64(rhs.digits[j]) + result[idx] + carry
                result[idx] = prod % 1000000000
                carry = prod / 1000000000
            }
            if carry > 0 { result[i + rhs.digits.count] += carry }
        }
        var resDigits: [UInt32] = result.map { UInt32($0) }
        while resDigits.last == 0 { resDigits.removeLast(); if resDigits.isEmpty { break } }
        var r = BigInt(); r.digits = resDigits; return r
    }
    func bitLength() -> Int {
        if digits.isEmpty { return 0 }
        var v = digits.last!
        var bits = 0
        while v > 0 { v >>= 1; bits += 1 }
        return bits + (digits.count - 1) * 30
    }
    static func <(lhs: BigInt, rhs: BigInt) -> Bool {
        if lhs.digits.count != rhs.digits.count { return lhs.digits.count < rhs.digits.count }
        for i in stride(from: lhs.digits.count-1, through: 0, by: -1) {
            if lhs.digits[i] != rhs.digits[i] { return lhs.digits[i] < rhs.digits[i] }
        }
        return false
    }
    static func >(lhs: BigInt, rhs: BigInt) -> Bool { return rhs < lhs }
    static func <=(lhs: BigInt, rhs: BigInt) -> Bool { return !(rhs < lhs) }
    static func >=(lhs: BigInt, rhs: BigInt) -> Bool { return !(lhs < rhs) }
    static func /(lhs: BigInt, rhs: BigInt) -> BigInt {
        return BigInt(lhs.toInt() / rhs.toInt())
    }
    static func %(lhs: BigInt, rhs: BigInt) -> BigInt {
        return BigInt(lhs.toInt() % rhs.toInt())
    }
    static func +(lhs: BigInt, rhs: Int) -> BigInt { return lhs + BigInt(rhs) }
    static func +(lhs: Int, rhs: BigInt) -> BigInt { return BigInt(lhs) + rhs }
    static func ==(lhs: BigInt, rhs: BigInt) -> Bool { lhs.digits == rhs.digits }
    var description: String {
        if digits.isEmpty { return "0" }
        var s = String(digits.last!)
        for d in digits.dropLast().reversed() { s += String(format: "%09d", d) }
        return s
    }
}
extension Int { init(_ b: BigInt) { self = b.toInt() } }
do {
    let _benchMemStart = _mem()
    let _benchStart = _now()
    func bellTriangle(_ n: Int) -> [[BigInt]] {
        var tri: [[BigInt]] = ([] as! [[BigInt]])
        var i: Int = 0
        while (i < n) {
            var row: [BigInt] = ([] as! [BigInt])
            var j: Int = 0
            while (j < i) {
                row = ((row + [BigInt(0)]) as! [BigInt])
                j = Int((j + 1))
            }
            tri = ((tri + [row]) as! [[BigInt]])
            i = Int((i + 1))
        }
        tri[1][0] = BigInt(1)
        i = 2
        while (i < n) {
            tri[i][0] = (tri[(i - 1)][(i - 2)] as! BigInt)
            var j: Int = 1
            while (j < i) {
                tri[i][j] = BigInt(((tri[i][(j - 1)] as! BigInt) + (tri[(i - 1)][(j - 1)] as! BigInt)))
                j = Int((j + 1))
            }
            i = Int((i + 1))
        }
        return (tri as! [[BigInt]])
    }
    func main() {
        let bt = (bellTriangle(51) as! [[BigInt]])
        print(_p("First fifteen and fiftieth Bell numbers:"))
        for i in 1..<16 {
            print(_p(((("" + String(describing: _padStart(_p(i), 2, " "))) + ": ") + _p((bt[i][0] as! BigInt)))))
        }
        print(_p(("50: " + _p((bt[50][0] as! BigInt)))))
        print(_p(""))
        print(_p("The first ten rows of Bell's triangle:"))
        for i in 1..<11 {
            print(_p("[" + (bt[i] as! [BigInt]).map{ String(describing: $0) }.joined(separator: ",") + "]"))
        }
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
