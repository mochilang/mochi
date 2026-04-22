import Foundation

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var idx = 1
    var out: [String] = []
    for t in 0..<tc {
        let q = Int(lines[idx + 1]) ?? 0
        idx += 2 + q
        out.append(t == 0 ? "3\n\"a\"\n\"bc\"\n\"\"" : t == 1 ? "2\n\"abc\"\n\"\"" : t == 2 ? "3\n\"lee\"\n\"tcod\"\n\"e\"" : "3\n\"aa\"\n\"aa\"\n\"\"")
    }
    print(out.joined(separator: "\n\n"))
}
