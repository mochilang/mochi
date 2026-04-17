import Foundation

func matchAt(_ s: [Character], _ p: [Character], _ i: Int, _ j: Int) -> Bool {
    if j >= p.count { return i >= s.count }
    let first = i < s.count && (p[j] == "." || s[i] == p[j])
    if j + 1 < p.count && p[j + 1] == "*" {
        return matchAt(s, p, i, j + 2) || (first && matchAt(s, p, i + 1, j))
    }
    return first && matchAt(s, p, i + 1, j + 1)
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty, let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) {
    var idx = 1
    var out: [String] = []
    for _ in 0..<t {
        let s = idx < lines.count ? lines[idx] : ""
        idx += 1
        let p = idx < lines.count ? lines[idx] : ""
        idx += 1
        out.append(matchAt(Array(s), Array(p), 0, 0) ? "true" : "false")
    }
    print(out.joined(separator: "\n"), terminator: "")
}
