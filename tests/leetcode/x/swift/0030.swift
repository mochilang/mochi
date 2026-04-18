import Foundation
func solveCase(_ s: String, _ words: [String]) -> [Int] {
    if words.isEmpty { return [] }
    let wlen = words[0].count
    let total = wlen * words.count
    let target = words.sorted()
    let chars = Array(s)
    if chars.count < total { return [] }
    var ans: [Int] = []
    for i in 0...(chars.count - total) {
        var parts: [String] = []
        for j in 0..<words.count { parts.append(String(chars[(i + j * wlen)..<(i + (j + 1) * wlen)])) }
        if parts.sorted() == target { ans.append(i) }
    }
    return ans
}
let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty, !lines[0].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
    var idx = 0
    let t = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0; idx += 1
    var out: [String] = []
    for _ in 0..<t {
        let s = idx < lines.count ? lines[idx] : ""; idx += 1
        let m = idx < lines.count ? (Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0) : 0; idx += 1
        var words: [String] = []
        for _ in 0..<m { words.append(idx < lines.count ? lines[idx] : ""); idx += 1 }
        out.append("[" + solveCase(s, words).map(String.init).joined(separator: ",") + "]")
    }
    print(out.joined(separator: "\n"), terminator: "")
}
