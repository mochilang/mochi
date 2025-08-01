// Generated by Mochi transpiler v0.10.54 on 2025-08-02 11:57:32 GMT+7
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

struct DivResult {
    var q: Int
    var ok: Bool
}
func divCheck(_ x: Int, _ y: Int) -> DivResult {
    if (y == 0) {
        return DivResult(q: 0, ok: false)
    }
    return DivResult(q: (x / y), ok: true)
}
func printResult(_ r: DivResult) {
    print(_p(((_p(r.q) + " ") + _p(r.ok))))
}
func main() {
    _ = printResult((divCheck(3, 2) as! DivResult))
    _ = printResult((divCheck(3, 0) as! DivResult))
}
_ = main()
