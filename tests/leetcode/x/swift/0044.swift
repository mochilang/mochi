import Foundation

func isMatch(_ s: String, _ p: String) -> Bool {
  let ss = Array(s)
  let pp = Array(p)
  var i = 0
  var j = 0
  var star = -1
  var match = 0
  while i < ss.count {
    if j < pp.count && (pp[j] == Character("?") || pp[j] == ss[i]) {
      i += 1
      j += 1
    } else if j < pp.count && pp[j] == Character("*") {
      star = j
      match = i
      j += 1
    } else if star != -1 {
      j = star + 1
      match += 1
      i = match
    } else {
      return false
    }
  }
  while j < pp.count && pp[j] == Character("*") { j += 1 }
  return j == pp.count
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false)
  .map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []

if !lines.isEmpty && !lines[0].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
  var idx = 0
  let t = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
  idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let n = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    idx += 1
    let s = n > 0 ? lines[idx] : ""
    if n > 0 { idx += 1 }
    let m = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    idx += 1
    let p = m > 0 ? lines[idx] : ""
    if m > 0 { idx += 1 }
    out.append(isMatch(s, p) ? "true" : "false")
  }
  print(out.joined(separator: "\n"), terminator: "")
}
