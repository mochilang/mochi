import Foundation
func minWindow(_ s: String, _ t: String) -> String {
  let sArr = Array(s.utf8); let tArr = Array(t.utf8)
  var need = Array(repeating: 0, count: 128)
  var missing = tArr.count
  for b in tArr { need[Int(b)] += 1 }
  var left = 0, bestStart = 0, bestLen = sArr.count + 1
  for right in sArr.indices {
    let c = Int(sArr[right])
    if need[c] > 0 { missing -= 1 }
    need[c] -= 1
    while missing == 0 {
      if right - left + 1 < bestLen { bestStart = left; bestLen = right - left + 1 }
      let lc = Int(sArr[left]); need[lc] += 1; if need[lc] > 0 { missing += 1 }; left += 1
    }
  }
  if bestLen > sArr.count { return "" }
  return String(decoding: sArr[bestStart..<bestStart+bestLen], as: UTF8.self)
}
let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty { let t = Int(lines[0]) ?? 0; var out:[String]=[]; for i in 0..<t { out.append(minWindow(lines[1+2*i], lines[2+2*i])) }; print(out.joined(separator: "\n"), terminator: "") }
