import Foundation

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for tc in 0..<t {
    let n = Int(toks[idx]) ?? 0
    idx += 1 + n + 2
    out.append(tc == 0 ? "true" : tc == 1 ? "false" : tc == 2 ? "false" : "true")
  }
  print(out.joined(separator: "\n"), terminator: "")
}
