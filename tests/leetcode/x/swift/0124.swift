import Foundation

func solve(_ vals: [Int], _ ok: [Bool]) -> Int {
    var best = -1_000_000_000
    func dfs(_ i: Int) -> Int {
        if i >= vals.count || !ok[i] { return 0 }
        let left = max(0, dfs(2 * i + 1))
        let right = max(0, dfs(2 * i + 2))
        best = max(best, vals[i] + left + right)
        return vals[i] + max(left, right)
    }
    _ = dfs(0)
    return best
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var idx = 1
    var out: [String] = []
    for _ in 0..<tc {
        let n = Int(lines[idx]) ?? 0
        idx += 1
        var vals = Array(repeating: 0, count: n)
        var ok = Array(repeating: false, count: n)
        for i in 0..<n {
            let tok = lines[idx]
            idx += 1
            if tok != "null" { ok[i] = true; vals[i] = Int(tok) ?? 0 }
        }
        out.append(String(solve(vals, ok)))
    }
    print(out.joined(separator: "\n"))
}
