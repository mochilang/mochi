import Foundation

func solve(_ dungeon: [[Int]]) -> Int {
  let cols = dungeon[0].count
  let inf = Int.max / 4
  var dp = Array(repeating: inf, count: cols + 1)
  dp[cols - 1] = 1
  for i in stride(from: dungeon.count - 1, through: 0, by: -1) {
    for j in stride(from: cols - 1, through: 0, by: -1) {
      let need = min(dp[j], dp[j + 1]) - dungeon[i][j]
      dp[j] = need <= 1 ? 1 : need
    }
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
    let cols = Int(toks[idx + 1]) ?? 0
    idx += 2
    var dungeon: [[Int]] = []
    for _ in 0..<rows {
      var row: [Int] = []
      for _ in 0..<cols { row.append(Int(toks[idx]) ?? 0); idx += 1 }
      dungeon.append(row)
    }
    out.append(String(solve(dungeon)))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
