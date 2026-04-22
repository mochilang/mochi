import Foundation

let pairs: [(Character, Character)] = [("0", "0"), ("1", "1"), ("6", "9"), ("8", "8"), ("9", "6")]

func build(_ n: Int, _ m: Int) -> [String] {
  if n == 0 { return [""] }
  if n == 1 { return ["0", "1", "8"] }
  let mids = build(n - 2, m)
  var res: [String] = []
  for mid in mids {
    for (a, b) in pairs {
      if n == m && a == "0" { continue }
      res.append(String(a) + mid + String(b))
    }
  }
  return res
}

func countRange(_ low: String, _ high: String) -> Int {
  var ans = 0
  for len in low.count...high.count {
    for s in build(len, len) {
      if len == low.count && s < low { continue }
      if len == high.count && s > high { continue }
      ans += 1
    }
  }
  return ans
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map(String.init) ?? []
if !lines.isEmpty, let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) {
  var out: [String] = []
  var idx = 1
  for _ in 0..<t {
    out.append(String(countRange(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines), lines[idx + 1].trimmingCharacters(in: .whitespacesAndNewlines))))
    idx += 2
  }
  print(out.joined(separator: "\n"), terminator: "")
}
