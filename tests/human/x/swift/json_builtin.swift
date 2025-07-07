import Foundation
let m = ["a": 1, "b": 2]
if let jsonData = try? JSONSerialization.data(withJSONObject: m, options: []),
   let jsonString = String(data: jsonData, encoding: .utf8) {
    print(jsonString)
}
