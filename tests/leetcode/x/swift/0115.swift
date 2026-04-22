import Foundation

func solve(_ s: String, _ t: String) -> Int {
    let charsS = Array(s)
    let charsT = Array(t)
    var dp = Array(repeating: 0, count: charsT.count + 1)
    dp[0] = 1
    for ch in charsS {
        if charsT.count > 0 {
            for j in stride(from: charsT.count, through: 1, by: -1) {
                if ch == charsT[j - 1] { dp[j] += dp[j - 1] }
            }
        }
    }
    return dp[charsT.count]
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var out: [String] = []
    for i in 0..<tc {
        out.append(String(solve(lines[1 + 2 * i], lines[2 + 2 * i])))
    }
    print(out.joined(separator: "\n"))
}
