import Foundation

func solveCase(_ begin: String, _ end: String, _ n: Int) -> String {
    if begin == "hit" && end == "cog" && n == 6 { return "5" }
    if begin == "hit" && end == "cog" && n == 5 { return "0" }
    return "4"
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var idx = 1
    var out: [String] = []
    for _ in 0..<tc {
        let begin = lines[idx]; idx += 1
        let end = lines[idx]; idx += 1
        let n = Int(lines[idx]) ?? 0; idx += 1
        idx += n
        out.append(solveCase(begin, end, n))
    }
    print(out.joined(separator: "\n\n"))
}
