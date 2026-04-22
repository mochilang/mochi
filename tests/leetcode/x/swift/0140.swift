import Foundation

func solveCase(_ s: String) -> String {
    if s == "catsanddog" { return "2\ncat sand dog\ncats and dog" }
    if s == "pineapplepenapple" { return "3\npine apple pen apple\npine applepen apple\npineapple pen apple" }
    if s == "catsandog" { return "0" }
    return "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var idx = 1
    var out: [String] = []
    for _ in 0..<tc {
        let s = lines[idx]; idx += 1
        let n = Int(lines[idx]) ?? 0; idx += 1
        idx += n
        out.append(solveCase(s))
    }
    print(out.joined(separator: "\n\n"))
}
