import Foundation

func solve(_ num: String, _ target: Int64) -> [String] {
  let chars = Array(num)
  var ans: [String] = []
  func dfs(_ i: Int, _ expr: String, _ value: Int64, _ last: Int64) {
    if i == chars.count {
      if value == target { ans.append(expr) }
      return
    }
    var j = i
    while j < chars.count {
      if j > i && chars[i] == "0" { break }
      let s = String(chars[i...j])
      let n = Int64(s)!
      if i == 0 {
        dfs(j + 1, s, n, n)
      } else {
        dfs(j + 1, expr + "+" + s, value + n, n)
        dfs(j + 1, expr + "-" + s, value - n, -n)
        dfs(j + 1, expr + "*" + s, value - last + last * n, last * n)
      }
      j += 1
    }
  }
  dfs(0, "", 0, 0)
  return ans.sorted()
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map(String.init) ?? []
if !lines.isEmpty, let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) {
  var blocks: [String] = []
  var idx = 1
  for _ in 0..<t {
    let num = lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)
    let target = Int64(lines[idx + 1].trimmingCharacters(in: .whitespacesAndNewlines))!
    idx += 2
    let ans = solve(num, target)
    blocks.append(([String(ans.count)] + ans).joined(separator: "\n"))
  }
  print(blocks.joined(separator: "\n\n"), terminator: "")
}
