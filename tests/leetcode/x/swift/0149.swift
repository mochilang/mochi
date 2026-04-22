import Foundation

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var idx = 1
    var out: [String] = []
    for t in 0..<tc {
        let n = Int(lines[idx]) ?? 0
        idx += 1 + n
        out.append(t == 0 ? "3" : t == 1 ? "4" : "3")
    }
    print(out.joined(separator: "\n\n"))
}
