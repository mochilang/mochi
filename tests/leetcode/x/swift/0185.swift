import Foundation

struct Row { let dept: String; let name: String; let salary: Int }

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx]) ?? 0
  idx += 1
  var cases: [String] = []
  for _ in 0..<t {
    let d = Int(toks[idx]) ?? 0
    let e = Int(toks[idx + 1]) ?? 0
    idx += 2
    var deptName: [Int: String] = [:]
    for _ in 0..<d { deptName[Int(toks[idx]) ?? 0] = toks[idx + 1]; idx += 2 }
    var groups: [Int: [(String, Int)]] = [:]
    for _ in 0..<e {
      idx += 1
      let name = toks[idx]
      let salary = Int(toks[idx + 1]) ?? 0
      let deptId = Int(toks[idx + 2]) ?? 0
      idx += 3
      groups[deptId, default: []].append((name, salary))
    }
    var rows: [Row] = []
    for (deptId, items) in groups {
      let keep = Set(items.map { $0.1 }).sorted(by: >).prefix(3)
      for (name, salary) in items where keep.contains(salary) {
        rows.append(Row(dept: deptName[deptId] ?? "", name: name, salary: salary))
      }
    }
    rows.sort { a, b in
      if a.dept != b.dept { return a.dept < b.dept }
      if a.salary != b.salary { return a.salary > b.salary }
      return a.name < b.name
    }
    cases.append(([String(rows.count)] + rows.map { "\($0.dept),\($0.name),\($0.salary)" }).joined(separator: "\n"))
  }
  print(cases.joined(separator: "\n\n"), terminator: "")
}
