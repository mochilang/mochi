import Foundation

func solve(_ tri: [[Int]]) -> Int {
  var dp = tri.last!
  if tri.count == 1 { return dp[0] }
  for i in stride(from: tri.count - 2, through: 0, by: -1) {
    for j in 0...i { dp[j] = tri[i][j] + min(dp[j], dp[j + 1]) }
  }
  return dp[0]
}

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let rows = Int(toks[idx]) ?? 0
    idx += 1
    var tri: [[Int]] = []
    for r in 1...rows {
      var row: [Int] = []
      for _ in 0..<r { row.append(Int(toks[idx]) ?? 0); idx += 1 }
      tri.append(row)
    }
    out.append(String(solve(tri)))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
