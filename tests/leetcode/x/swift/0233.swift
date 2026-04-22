import Foundation

func countDigitOne(_ n: Int64) -> Int64 {
  var total: Int64 = 0
  var m: Int64 = 1
  while m <= n {
    let high = n / (m * 10)
    let cur = (n / m) % 10
    let low = n % m
    if cur == 0 {
      total += high * m
    } else if cur == 1 {
      total += high * m + low + 1
    } else {
      total += (high + 1) * m
    }
    m *= 10
  }
  return total
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map(String.init) ?? []
if !lines.isEmpty, let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) {
  var out: [String] = []
  for i in 0..<t {
    out.append(String(countDigitOne(Int64(lines[i + 1].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0)))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
