import Foundation

func longest(_ s: String) -> Int {
    let chars = Array(s)
    var last: [Character: Int] = [:]
    var left = 0
    var best = 0
    for right in 0..<chars.count {
        let ch = chars[right]
        if let pos = last[ch], pos >= left {
            left = pos + 1
        }
        last[ch] = right
        best = max(best, right - left + 1)
    }
    return best
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map {
    String($0).replacingOccurrences(of: "\r", with: "")
}
if !lines.isEmpty && !lines[0].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
    let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines))!
    let out = (0..<t).map { i in String(longest(i + 1 < lines.count ? lines[i + 1] : "")) }
    print(out.joined(separator: "\n"))
}
