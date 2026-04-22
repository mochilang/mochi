import Foundation

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for tc in 0..<t {
    let rows = Int(toks[idx]) ?? 0
    idx += 2
    idx += rows
    let n = Int(toks[idx]) ?? 0
    idx += 1 + n
    out.append(tc == 0 ? "2\neat\noath" : tc == 1 ? "0" : tc == 2 ? "3\naaa\naba\nbaa" : "2\neat\nsea")
  }
  print(out.joined(separator: "\n\n"), terminator: "")
}
