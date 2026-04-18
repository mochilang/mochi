import Foundation

func maxArea(_ h: [Int]) -> Int {
    var left = 0, right = h.count - 1, best = 0
    while left < right {
        let height = min(h[left], h[right])
        best = max(best, (right - left) * height)
        if h[left] < h[right] { left += 1 } else { right -= 1 }
    }
    return best
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var idx = 1
    var out: [String] = []
    for _ in 0..<t {
        let n = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
        idx += 1
        var h: [Int] = []
        for _ in 0..<n {
            h.append(Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0)
            idx += 1
        }
        out.append(String(maxArea(h)))
    }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
