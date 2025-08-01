// Generated by Mochi transpiler v0.10.41 on 2025-07-27 16:10:35 GMT+7
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
    func indexOf(_ s: String, _ ch: String) -> Int {
        var i: Int = 0
        while (i < Int(((s).count))) {
            if ((String(Array(s)[i]) as! String) == ch) {
                return Int(i)
            }
            i = Int((i + 1))
        }
        return Int(-1)
    }
    func parseIntStr(_ str: String) -> Int {
        var i: Int = 0
        var neg: Bool = false
        if ((Int(((str).count)) > 0) && ((String(Array(str)[0]) as! String) == "-")) {
            neg = true
            i = 1
        }
        var n: Int = 0
        let digits: [String: Any] = ["0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9]
        while (i < Int(((str).count))) {
            n = Int(((n * 10) + ((digits as! [String: Any])[(String(Array(str)[i]) as! String)] as! Int)))
            i = Int((i + 1))
        }
        if neg {
            n = Int(-n)
        }
        return Int(n)
    }
    func ord(_ ch: String) -> Int {
        let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower = "abcdefghijklmnopqrstuvwxyz"
        var idx: Int = Int((String(describing: String(upper)).firstIndex(of: Character(String(describing: String(ch))))?.utf16Offset(in: String(describing: String(upper)) ) ?? -1))
        if (idx >= 0) {
            return Int((65 + idx))
        }
        idx = Int((String(describing: String(lower)).firstIndex(of: Character(String(describing: String(ch))))?.utf16Offset(in: String(describing: String(lower)) ) ?? -1))
        if (idx >= 0) {
            return Int((97 + idx))
        }
        if ((ch >= "0") && (ch <= "9")) {
            return Int((48 + Int(Int(String(describing: String(ch)))!)))
        }
        if (ch == "+") {
            return 43
        }
        if (ch == "/") {
            return 47
        }
        if (ch == " ") {
            return 32
        }
        if (ch == "=") {
            return 61
        }
        return 0
    }
    func chr(_ n: Int) -> String {
        let upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let lower = "abcdefghijklmnopqrstuvwxyz"
        if ((n >= 65) && (n < 91)) {
            return String(String(Array(upper)[(n - 65)..<(n - 64)]))
        }
        if ((n >= 97) && (n < 123)) {
            return String(String(Array(lower)[(n - 97)..<(n - 96)]))
        }
        if ((n >= 48) && (n < 58)) {
            let digits = "0123456789"
            return String(String(Array(digits)[(n - 48)..<(n - 47)]))
        }
        if (n == 43) {
            return "+"
        }
        if (n == 47) {
            return "/"
        }
        if (n == 32) {
            return " "
        }
        if (n == 61) {
            return "="
        }
        return "?"
    }
    func toBinary(_ n: Int, _ bits: Int) -> String {
        var b: String = ""
        var val: Int = n
        var i: Int = 0
        while (i < bits) {
            b = String((_p((val % 2)) + b))
            val = Int((val / 2))
            i = Int((i + 1))
        }
        return String(b)
    }
    func binToInt(_ bits: String) -> Int {
        var n: Int = 0
        var i: Int = 0
        while (i < Int(((bits).count))) {
            n = Int(((n * 2) + Int(Int(String(describing: String(String(Array(bits)[i..<(i + 1)]))))!)))
            i = Int((i + 1))
        }
        return Int(n)
    }
    func base64Encode(_ text: String) -> String {
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        var bin: String = ""
        for ch in text {
            bin = String((bin + String(describing: toBinary(Int(ord(String(ch))), 8))))
        }
        while ((Int(((bin).count)) % 6) != 0) {
            bin = String((bin + "0"))
        }
        var out: String = ""
        var i: Int = 0
        while (i < Int(((bin).count))) {
            let chunk = String(Array(bin)[i..<(i + 6)])
            let val = Int(binToInt(String(chunk)))
            out = String((out + String(Array(alphabet)[val..<(val + 1)])))
            i = Int((i + 6))
        }
        let pad = (Int((3 - Int((Int(((text).count)) % 3)))) % 3)
        if (pad == 1) {
            out = String((String(Array(out)[0..<(Int(((out).count)) - 1)]) + "="))
        }
        if (pad == 2) {
            out = String((String(Array(out)[0..<(Int(((out).count)) - 2)]) + "=="))
        }
        return String(out)
    }
    func base64Decode(_ enc: String) -> String {
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        var bin: String = ""
        var i: Int = 0
        while (i < Int(((enc).count))) {
            let ch = (String(Array(enc)[i]) as! String)
            if (ch == "=") {
                break
            }
            let idx = Int((String(describing: String(alphabet)).firstIndex(of: Character(String(describing: String(ch))))?.utf16Offset(in: String(describing: String(alphabet)) ) ?? -1))
            bin = String((bin + String(describing: toBinary(Int(idx), 6))))
            i = Int((i + 1))
        }
        var out: String = ""
        i = 0
        while ((i + 8) <= Int(((bin).count))) {
            let chunk = String(Array(bin)[i..<(i + 8)])
            let val = Int(binToInt(String(chunk)))
            out = String((out + String(describing: chr(Int(val)))))
            i = Int((i + 8))
        }
        return String(out)
    }
    let msg = "Rosetta Code Base64 decode data task"
    print(_p(("Original : " + msg)))
    let enc = String(describing: base64Encode(String(msg)))
    print(_p(("\nEncoded  : " + enc)))
    let dec = String(describing: base64Decode(String(enc)))
    print(_p(("\nDecoded  : " + dec)))
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
