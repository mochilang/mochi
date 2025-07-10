import Foundation

func _parseVal(_ s: String) -> Any {
    if let i = Int(s) { return i }
    if let d = Double(s) { return d }
    return s
}

func _load(path: String, opts: [String:Any]?) -> [[String:Any]] {
    var text = ""
    if path.isEmpty || path == "-" {
        text = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
    } else {
        text = (try? String(contentsOfFile: path)) ?? ""
    }
    var rows: [[String:Any]] = []
    var cur: [String:Any] = [:]
    for line in text.split(separator: "\n") {
        let l = line.trimmingCharacters(in: .whitespaces)
        if l.hasPrefix("- ") {
            if !cur.isEmpty { rows.append(cur); cur = [:] }
            let rest = String(l.dropFirst(2))
            if let idx = rest.firstIndex(of: ":") {
                let key = String(rest[..<idx]).trimmingCharacters(in: .whitespaces)
                let val = String(rest[rest.index(after: idx)...]).trimmingCharacters(in: .whitespaces)
                cur[key] = _parseVal(val)
            }
        } else if let idx = l.firstIndex(of: ":") {
            let key = String(l[..<idx]).trimmingCharacters(in: .whitespaces)
            let val = String(l[l.index(after: idx)...]).trimmingCharacters(in: .whitespaces)
            cur[key] = _parseVal(val)
        }
    }
    if !cur.isEmpty { rows.append(cur) }
    return rows
}
struct Person: Equatable {
    var name: String
    var age: Int
    var email: String
}
let people = _load(path: "../interpreter/valid/people.yaml", opts: ["format": "yaml"]).map { rec in Person(name: rec["name"] as! String, age: rec["age"] as! Int, email: rec["email"] as! String) }
var adults = people.compactMap { p in p.age >= 18 ? (["name": p.name, "email": p.email]) : nil }
for a in adults {
    print(a["name"], a["email"])
}
