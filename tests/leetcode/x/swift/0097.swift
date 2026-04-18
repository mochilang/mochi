import Foundation

func solve(_ s1: String, _ s2: String, _ s3: String) -> Bool {
  let a1 = Array(s1), a2 = Array(s2), a3 = Array(s3)
  let m = a1.count, n = a2.count
  if m + n != a3.count { return false }
  var dp = Array(repeating: Array(repeating: false, count: n + 1), count: m + 1)
  dp[0][0] = true
  for i in 0...m {
    for j in 0...n {
      if i > 0 && dp[i - 1][j] && a1[i - 1] == a3[i + j - 1] { dp[i][j] = true }
      if j > 0 && dp[i][j - 1] && a2[j - 1] == a3[i + j - 1] { dp[i][j] = true }
    }
  }
  return dp[m][n]
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty && !lines[0].isEmpty {
  let t = Int(lines[0].trimmingCharacters(in: .whitespaces)) ?? 0
  var out: [String] = []
  for i in 0..<t { out.append(solve(lines[1 + 3*i], lines[2 + 3*i], lines[3 + 3*i]) ? "true" : "false") }
  print(out.joined(separator: "\n"), terminator: "")
}
