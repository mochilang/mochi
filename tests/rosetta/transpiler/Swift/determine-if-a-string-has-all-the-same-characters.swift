// Generated by Mochi transpiler v0.10.54 on 2025-08-02 12:10:22 GMT+7
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

func ord(_ ch: String) -> Int {
    if (ch == "5") {
        return 53
    }
    if (ch == "T") {
        return 84
    }
    if (ch == " ") {
        return 32
    }
    if (ch == "é") {
        return 233
    }
    if (ch == "🐺") {
        return 128058
    }
    return 0
}
func hex(_ n: Int) -> String {
    let digits = "0123456789abcdef"
    if (n == 0) {
        return "0x0"
    }
    var m: Int = n
    var out: String = ""
    while (m > 0) {
        let d = (m % 16)
        out = ((String(describing: String(Array(String(describing: (digits as! String)))[Int(d)..<Int((d &+ 1))])) + out) as! String)
        m = Int((m / 16))
    }
    return ("0x" + out)
}
func quote(_ s: String) -> String {
    return (("'" + s) + "'")
}
func analyze(_ s: String) {
    let le = Int(((s).count))
    print(_p((((("Analyzing " + String(describing: quote((s as! String)))) + " which has a length of ") + _p(le)) + ":")))
    if (le > 1) {
        var i: Int = 1
        while (i < le) {
            let cur = String(describing: String(Array(String(describing: (s as! String)))[Int(i)..<Int((i &+ 1))]))
            let prev = String(describing: String(Array(String(describing: (s as! String)))[Int((i &- 1))..<Int(i)]))
            if (cur != prev) {
                print(_p("  Not all characters in the string are the same."))
                print(_p((((((("  " + String(describing: quote((cur as! String)))) + " (") + String(describing: hex(Int(ord((cur as! String)))))) + ") is different at position ") + _p((i &+ 1))) + ".")))
                print(_p(""))
                return
            }
            i = Int((i &+ 1))
        }
    }
    print(_p("  All characters in the string are the same."))
    print(_p(""))
}
func main() {
    let strings: [String] = (["", "   ", "2", "333", ".55", "tttTTT", "4444 444k", "pépé", "🐶🐶🐺🐶", "🎄🎄🎄🎄"] as! [String])
    var i: Int = 0
    while (i < Int(((strings).count))) {
        _ = analyze((strings[i] as! String))
        i = Int((i &+ 1))
    }
}
_ = main()
