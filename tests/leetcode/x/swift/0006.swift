import Foundation

func convertZigzag(_ s: String, _ numRows: Int) -> String {
    let chars = Array(s)
    if numRows <= 1 || numRows >= chars.count { return s }
    let cycle = 2 * numRows - 2
    var out = ""
    for row in 0..<numRows {
        var i = row
        while i < chars.count {
            out.append(chars[i])
            let diag = i + cycle - 2 * row
            if row > 0 && row < numRows - 1 && diag < chars.count {
                out.append(chars[diag])
            }
            i += cycle
        }
    }
    return out
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var out: [String] = []
    var idx = 1
    for _ in 0..<t {
        let s = idx < lines.count ? lines[idx] : ""
        idx += 1
        let r = idx < lines.count ? (Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 1) : 1
        idx += 1
        out.append(convertZigzag(s, r))
    }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
