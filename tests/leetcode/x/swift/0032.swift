import Foundation

func solveCase(_ s: String) -> Int {
    let chars = Array(s)
    var stack = [-1]
    var best = 0
    for i in 0..<chars.count {
        if chars[i] == "(" {
            stack.append(i)
        } else {
            stack.removeLast()
            if stack.isEmpty {
                stack.append(i)
            } else {
                best = max(best, i - stack.last!)
            }
        }
    }
    return best
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty, !lines[0].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
    var idx = 0
    let t = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    idx += 1
    var out: [String] = []
    for _ in 0..<t {
        let n = idx < lines.count ? (Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0) : 0
        idx += 1
        let s = (n > 0 && idx < lines.count) ? lines[idx] : ""
        if n > 0 { idx += 1 }
        out.append(String(solveCase(s)))
    }
    print(out.joined(separator: "\n"), terminator: "")
}
