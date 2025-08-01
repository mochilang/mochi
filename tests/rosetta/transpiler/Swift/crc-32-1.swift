// Generated by Mochi transpiler v0.10.55 on 2025-08-02 21:13:38 GMT+7
import Foundation

let stdout = FileHandle.standardOutput
extension FileHandle {
    func write(_ string: String) {
        if let data = string.data(using: .utf8) {
            self.write(data)
        }
    }
}

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

extension Double { init(_ v: Any) { if let d = v as? Double { self = d } else if let i = v as? Int { self = Double(i) } else if let i = v as? Int64 { self = Double(i) } else if let s = v as? String { self = Double(s) ?? 0 } else { self = 0 } } }
func _append<T>(_ xs: [T], _ v: T) -> [T] {
    var out = xs
    out.append(v)
    return out
}
func xor(_ a: Int, _ b: Int) -> Int {
    var res: Int = 0
    var bit: Int = 1
    var x: Int = a
    var y: Int = b
    while ((x > 0) || (y > 0)) {
        let abit = (x % 2)
        let bbit = (y % 2)
        if (abit != bbit) {
            res = Int((res &+ bit))
        }
        x = Int((x / 2))
        y = Int((y / 2))
        bit = Int((bit &* 2))
    }
    return res
}
func rshift(_ x: Int, _ n: Int) -> Int {
    var v: Int = x
    var i: Int = 0
    while (i < n) {
        v = Int((v / 2))
        i = Int((i &+ 1))
    }
    return v
}
func ord(_ ch: String) -> Int {
    let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let lower = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = Int((String(describing: (upper as! String)).range(of: String(describing: (ch as! String)))?.lowerBound.utf16Offset(in: String(describing: (upper as! String))) ?? -1))
    if (idx >= 0) {
        return (65 &+ idx)
    }
    idx = Int((String(describing: (lower as! String)).range(of: String(describing: (ch as! String)))?.lowerBound.utf16Offset(in: String(describing: (lower as! String))) ?? -1))
    if (idx >= 0) {
        return (97 &+ idx)
    }
    if (ch == " ") {
        return 32
    }
    return 0
}
func toHex(_ n: Int) -> String {
    let digits = "0123456789ABCDEF"
    if (n == 0) {
        return "0"
    }
    var v: Int = n
    var out: String = ""
    while (v > 0) {
        let d = (v % 16)
        out = ((String(Array(digits)[d..<(d &+ 1)]) + out) as! String)
        v = Int((v / 16))
    }
    return out
}
func crc32Table() -> [Int] {
    var table: [Int] = ([] as! [Int])
    var i: Int = 0
    while (i < 256) {
        var word: Int = i
        var j: Int = 0
        while (j < 8) {
            if ((word % 2) == 1) {
                word = Int(xor(Int(rshift((word as! Int), 1)), 3988292384))
            } else {
                word = Int(rshift((word as! Int), 1))
            }
            j = Int((j &+ 1))
        }
        table = (_append(table, word) as! [Int])
        i = Int((i &+ 1))
    }
    return table
}
var table: [Int] = (crc32Table() as! [Int])
func crc32(_ s: String) -> Int {
    var crc: Int = 4294967295
    var i: Int = 0
    while (i < Int(((s).count))) {
        let c = Int(ord((String(Array(s)[i..<(i &+ 1)]) as! String)))
        let idx = Int(xor(Int((crc % 256)), (c as! Int)))
        crc = Int(xor((table[idx] as! Int), Int(rshift((crc as! Int), 8))))
        i = Int((i &+ 1))
    }
    return (4294967295 &- crc)
}
func main() {
    let s = "The quick brown fox jumps over the lazy dog"
    let result = Int(crc32((s as! String)))
    let hex = String(describing: toHex((result as! Int)))
    print(_p(hex))
}
_ = main()
