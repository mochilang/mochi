import Foundation

func isScramble(_ s1: String, _ s2: String) -> Bool {
  let a1 = Array(s1), a2 = Array(s2)
  var memo: [String: Bool] = [:]
  func dfs(_ i1: Int, _ i2: Int, _ len: Int) -> Bool {
    let key = "\(i1),\(i2),\(len)"
    if let v = memo[key] { return v }
    if String(a1[i1..<i1+len]) == String(a2[i2..<i2+len]) { memo[key] = true; return true }
    var cnt = Array(repeating: 0, count: 26)
    for i in 0..<len {
      cnt[Int(a1[i1 + i].asciiValue! - 97)] += 1
      cnt[Int(a2[i2 + i].asciiValue! - 97)] -= 1
    }
    if cnt.contains(where: { $0 != 0 }) { memo[key] = false; return false }
    for k in 1..<len {
      if (dfs(i1, i2, k) && dfs(i1 + k, i2 + k, len - k)) || (dfs(i1, i2 + len - k, k) && dfs(i1 + k, i2, len - k)) {
        memo[key] = true; return true
      }
    }
    memo[key] = false
    return false
  }
  return dfs(0, 0, s1.count)
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty && !lines[0].isEmpty {
  let t = Int(lines[0]) ?? 0
  var out: [String] = []
  for i in 0..<t { out.append(isScramble(lines[1 + 2 * i], lines[2 + 2 * i]) ? "true" : "false") }
  print(out.joined(separator: "\n"), terminator: "")
}
