// Generated by Mochi transpiler v0.10.52 on 2025-08-02 01:28:32 GMT+7
import Foundation

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

func _int(_ v: Any) -> Int {
    if let s = v as? String { return Int(s) ?? 0 }
    if let i = v as? Int { return i }
    if let i = v as? Int64 { return Int(i) }
    if let d = v as? Double { return Int(d) }
    return 0
}
func pow(_ base: Double, _ exp: Double) -> Double {
    var result: Double = 1.0
    var i: Int = 0
    while (i < Int(_int(exp))) {
        result = Double((result * base))
        i = ((i &+ 1) as! Int)
    }
    return result
}
func PowN(_ b: Double) -> (Double) -> Double {
    return { (e: Double) -> Double in Double(pow((b as! Double), (e as! Double))) }
}
func PowE(_ e: Double) -> (Double) -> Double {
    return { (b: Double) -> Double in Double(pow((b as! Double), (e as! Double))) }
}
struct Foo {
    var value: Int
}
func Foo_Method(_ self: Foo, _ b: Int) -> Int {
    return (self.value &+ b)
}
func main() {
    let pow2 = PowN(2.0)
    let cube = PowE(3.0)
    print(_p(("2^8 = " + _p(Double(pow2(8.0))))))
    print(_p(("4³ = " + _p(Double(cube(4.0))))))
    var a: Foo = Foo(value: 2)
    let fn1 = { (b: Int) -> Int in Foo_Method(a, b) }
    let fn2 = { (f: Foo, b: Int) -> Int in Foo_Method(f, b) }
    print(_p(("2 + 2 = " + _p(Foo_Method(a, 2)))))
    print(_p(("2 + 3 = " + _p(Int(fn1(3))))))
    print(_p(("2 + 4 = " + _p(Int(fn2((a as! Foo), 4))))))
    print(_p(("3 + 5 = " + _p(Int(fn2((Foo(value: 3) as! Foo), 5))))))
}
_ = main()
