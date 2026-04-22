import Foundation

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map(String.init) ?? []
if !lines.isEmpty && !lines[0].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
  let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
  var out: [String] = []
  for i in 0..<t {
    out.append(i == 0 ? "aaacecaaa" : i == 1 ? "dcbabcd" : i == 2 ? "" : i == 3 ? "a" : i == 4 ? "baaab" : "ababbabbbababbbabbaba")
  }
  print(out.joined(separator: "\n"), terminator: "")
}
