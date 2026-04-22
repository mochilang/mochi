import Foundation

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)!
    .split(whereSeparator: \.isNewline)
    .map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
    .filter { !$0.isEmpty }

if !lines.isEmpty {
    let t = Int(lines[0])!
    var idx = 1
    var blocks: [String] = []
    for _ in 0..<t {
        let parts = lines[idx].split(separator: " ").map(String.init)
        idx += 1
        let r = Int(parts[0])!
        let image = Array(lines[idx..<idx + r])
        idx += r
        idx += 1
        var top = r
        var bottom = -1
        var left = image[0].count
        var right = -1
        for i in 0..<image.count {
            let chars = Array(image[i])
            for j in 0..<chars.count where chars[j] == "1" {
                top = min(top, i)
                bottom = max(bottom, i)
                left = min(left, j)
                right = max(right, j)
            }
        }
        blocks.append(String((bottom - top + 1) * (right - left + 1)))
    }
    print(blocks.joined(separator: "\n\n"), terminator: "")
}
