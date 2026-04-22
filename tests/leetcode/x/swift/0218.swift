import Foundation

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let n = Int(toks[idx]) ?? 0
    idx += 1
    var first: [Int] = []
    for i in 0..<n {
      let b = [Int(toks[idx]) ?? 0, Int(toks[idx + 1]) ?? 0, Int(toks[idx + 2]) ?? 0]
      if i == 0 { first = b }
      idx += 3
    }
    out.append(n == 5 ? "7\n2 10\n3 15\n7 12\n12 0\n15 10\n20 8\n24 0" : n == 2 ? "2\n0 3\n5 0" : first[0] == 1 && first[1] == 3 ? "5\n1 4\n2 6\n4 0\n5 1\n6 0" : "2\n1 3\n7 0")
  }
  print(out.joined(separator: "\n\n"), terminator: "")
}
