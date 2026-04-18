import Foundation

func getPermutation(_ n: Int, _ kInput: Int) -> String {
  var digits = (1...n).map { String($0) }
  var fact = Array(repeating: 1, count: n + 1)
  if n >= 1 {
    for i in 1...n { fact[i] = fact[i - 1] * i }
  }
  var k = kInput - 1
  var out = ""
  for rem in stride(from: n, through: 1, by: -1) {
    let block = fact[rem - 1]
    let idx = k / block
    k %= block
    out += digits.remove(at: idx)
  }
  return out
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false)
  .map { String($0).replacingOccurrences(of: "\r", with: "").trimmingCharacters(in: .whitespacesAndNewlines) } ?? []
if !lines.isEmpty && !lines[0].isEmpty {
  var idx = 0
  let t = Int(lines[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let n = Int(lines[idx]) ?? 0
    idx += 1
    let k = Int(lines[idx]) ?? 0
    idx += 1
    out.append(getPermutation(n, k))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
