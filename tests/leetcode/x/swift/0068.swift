import Foundation
func justify(_ words: [String], _ maxWidth: Int) -> [String] {
  var res: [String] = []
  var i = 0
  while i < words.count {
    var j = i
    var total = 0
    while j < words.count && total + words[j].count + (j - i) <= maxWidth { total += words[j].count; j += 1 }
    let gaps = j - i - 1
    let line: String
    if j == words.count || gaps == 0 {
      let joined = words[i..<j].joined(separator: " ")
      line = joined + String(repeating: " ", count: maxWidth - joined.count)
    } else {
      let spaces = maxWidth - total, base = spaces / gaps, extra = spaces % gaps
      var s = ""
      for k in i..<j-1 { s += words[k] + String(repeating: " ", count: base + (k - i < extra ? 1 : 0)) }
      s += words[j - 1]
      line = s
    }
    res.append(line); i = j
  }
  return res
}
let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty { var idx = 0; let t = Int(lines[idx]) ?? 0; idx += 1; var out: [String] = []; for tc in 0..<t { let n = Int(lines[idx]) ?? 0; idx += 1; let words = Array(lines[idx..<idx+n]); idx += n; let width = Int(lines[idx]) ?? 0; idx += 1; let ans = justify(words, width); out.append(String(ans.count)); for s in ans { out.append("|\(s)|") }; if tc + 1 < t { out.append("=") } }; print(out.joined(separator: "\n"), terminator: "") }
