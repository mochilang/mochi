import Foundation

func solve(_ a: [Int]) -> Int {
  var best = 0
  for i in 0..<a.count {
    var mn = a[i]
    for j in i..<a.count {
      if a[j] < mn { mn = a[j] }
      let area = mn * (j - i + 1)
      if area > best { best = area }
    }
  }
  return best
}

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }
  .map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let n = Int(toks[idx]) ?? 0
    idx += 1
    var a: [Int] = []
    for _ in 0..<n { a.append(Int(toks[idx]) ?? 0); idx += 1 }
    out.append(String(solve(a)))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
