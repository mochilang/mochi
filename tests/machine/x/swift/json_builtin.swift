import Foundation

func _json(_ v: Any) {
    if let d = try? JSONSerialization.data(withJSONObject: v, options: []),
       let s = String(data: d, encoding: .utf8) {
        print(s)
    }
}
let m = ["a": 1, "b": 2]
_json(m)
