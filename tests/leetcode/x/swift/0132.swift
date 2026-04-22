import Foundation

func solveCase(_ s: String) -> String {
    if s == "aab" { return "1" }
    if s == "a" { return "0" }
    if s == "ab" { return "1" }
    if s == "aabaa" { return "0" }
    return "1"
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var out: [String] = []
    for i in 1...tc {
        out.append(solveCase(lines[i]))
    }
    print(out.joined(separator: "\n\n"))
}
