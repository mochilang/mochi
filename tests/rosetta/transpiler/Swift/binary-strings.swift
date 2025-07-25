// Generated by Mochi transpiler v0.10.41 on 2025-07-26 23:11:45 GMT+7
import Foundation

func _p(_ v: Any?) -> String {
    if let val = v { return String(describing: val) }
    return "<nil>"
}

func char(_ n: Int) -> String {
    let letters = "abcdefghijklmnopqrstuvwxyz"
    let idx = (n - 97)
    if ((idx < 0) || (idx >= Int(((letters).count)))) {
        return "?"
    }
    return String(describing: String(Array(String(describing: String(letters)))[Int(idx)..<Int((idx + 1))]))
}
func fromBytes(_ bs: [Int]) -> String {
    var s: String = ""
    var i: Int = 0
    while (i < Int(((bs).count))) {
        s = String((s + String(describing: char((bs[i] as! Int)))))
        i = Int((i + 1))
    }
    return String(s)
}
var b: [Int] = ([98, 105, 110, 97, 114, 121] as! [Int])
print(_p(_p(b)))
var c: [Int] = b
print(_p(_p(c)))
print(_p(_p((b == c))))
var d: [Int] = ([] as! [Int])
var i: Int = 0
while (i < Int(((b).count))) {
    d = ((d + [(b[i] as! Int)]) as! [Int])
    i = Int((i + 1))
}
d[1] = 97
d[4] = 110
print(_p(String(describing: fromBytes((b as! [Int])))))
print(_p(String(describing: fromBytes((d as! [Int])))))
print(_p(_p((Int(((b).count)) == 0))))
var z: [Int] = ((b + [122]) as! [Int])
print(_p(String(describing: fromBytes((z as! [Int])))))
var sub: [Int] = (Array(b[1..<3]) as! [Int])
print(_p(String(describing: fromBytes((sub as! [Int])))))
var f: [Int] = ([] as! [Int])
i = 0
while (i < Int(((d).count))) {
    let val = (d[i] as! Int)
    if (val == 110) {
        f = ((f + [109]) as! [Int])
    } else {
        f = ((f + [val]) as! [Int])
    }
    i = Int((i + 1))
}
print(_p(((String(describing: fromBytes((d as! [Int]))) + " -> ") + String(describing: fromBytes((f as! [Int]))))))
var rem: [Int] = ([] as! [Int])
rem = ((rem + [(b[0] as! Int)]) as! [Int])
i = 3
while (i < Int(((b).count))) {
    rem = ((rem + [(b[i] as! Int)]) as! [Int])
    i = Int((i + 1))
}
print(_p(String(describing: fromBytes((rem as! [Int])))))
