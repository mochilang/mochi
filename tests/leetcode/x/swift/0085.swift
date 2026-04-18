import Foundation

func hist(_ h: [Int]) -> Int {
  var best = 0
  for i in 0..<h.count {
    var mn = h[i]
    for j in i..<h.count {
      if h[j] < mn { mn = h[j] }
      let area = mn * (j - i + 1)
      if area > best { best = area }
    }
  }
  return best
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false)
  .map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty && !lines[0].isEmpty {
  let t = Int(lines[0].trimmingCharacters(in: .whitespaces)) ?? 0
  var idx = 1
  var out: [String] = []
  for _ in 0..<t {
    let rc = lines[idx].split(separator: " ").map(String.init)
    idx += 1
    let rows = Int(rc[0]) ?? 0
    let cols = Int(rc[1]) ?? 0
    var h = Array(repeating: 0, count: cols)
    var best = 0
    for _ in 0..<rows {
      let s = lines[idx]
      idx += 1
      for c in 0..<cols { h[c] = Array(s)[c] == "1" ? h[c] + 1 : 0 }
      best = max(best, hist(h))
    }
    out.append(String(best))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
