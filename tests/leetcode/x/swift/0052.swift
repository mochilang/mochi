import Foundation
func solve(_ n: Int) -> [[String]] {
  var cols = Array(repeating: false, count: n)
  var d1 = Array(repeating: false, count: 2 * n)
  var d2 = Array(repeating: false, count: 2 * n)
  var board = Array(repeating: String(repeating: ".", count: n), count: n)
  var res: [[String]] = []
  func dfs(_ r: Int) {
    if r == n { res.append(board); return }
    for c in 0..<n { let a = r + c; let b = r - c + n - 1; if cols[c] || d1[a] || d2[b] { continue }; cols[c] = true; d1[a] = true; d2[b] = true; board[r] = String(repeating: ".", count: c) + "Q" + String(repeating: ".", count: n - c - 1); dfs(r + 1); board[r] = String(repeating: ".", count: n); cols[c] = false; d1[a] = false; d2[b] = false }
  }
  dfs(0); return res
}
let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false)
  .map { String($0).replacingOccurrences(of: "\r", with: "").trimmingCharacters(in: .whitespacesAndNewlines) } ?? []
if !lines.isEmpty && !lines[0].isEmpty {
  var idx = 0
  let t = Int(lines[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for tc in 0..<t {
    let n = Int(lines[idx]) ?? 0
    idx += 1
    let sols = solve(n)
    out.append(String(sols.count))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
