// Generated by Mochi transpiler v0.10.55 on 2025-08-02 21:13:58 GMT+7
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
func listStr(_ xs: [Int]) -> String {
    var s: String = "["
    var i: Int = 0
    while (i < Int(((xs).count))) {
        s = ((s + String(describing: (xs[i] as! Int))) as! String)
        if (i < (Int(((xs).count)) &- 1)) {
            s = ((s + " ") as! String)
        }
        i = Int((i &+ 1))
    }
    s = ((s + "]") as! String)
    return s
}
func pointerDemo() {
    print(_p("Pointer:"))
    var i: Int = 0
    print(_p("Before:"))
    print(_p(((("\t<address>: " + String(describing: i)) + ", ") + String(describing: i))))
    i = 3
    print(_p("After:"))
    print(_p(((("\t<address>: " + String(describing: i)) + ", ") + String(describing: i))))
}
func sliceDemo() {
    print(_p("Slice:"))
    var a: [Any?] = []
    for _ in 0..<10 {
        a = ((_append(a, 0) as! [Int]) as! [Any?])
    }
    var s: [Any?] = a
    print(_p("Before:"))
    print(_p(("\ts: " + String(describing: listStr((s as! [Int]))))))
    print(_p(("\ta: " + String(describing: listStr((a as! [Int]))))))
    var data: [Int] = ([65, 32, 115, 116, 114, 105, 110, 103, 46] as! [Int])
    var idx: Int = 0
    while (idx < Int(((data).count))) {
        s[idx] = (data[idx] as! Int)
        idx = Int((idx &+ 1))
    }
    print(_p("After:"))
    print(_p(("\ts: " + String(describing: listStr((s as! [Int]))))))
    print(_p(("\ta: " + String(describing: listStr((a as! [Int]))))))
}
_ = pointerDemo()
print(_p(""))
_ = sliceDemo()
