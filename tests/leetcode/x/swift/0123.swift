import Foundation

func maxProfit(_ prices: [Int]) -> Int {
    var buy1 = -1_000_000_000
    var sell1 = 0
    var buy2 = -1_000_000_000
    var sell2 = 0
    for p in prices {
        buy1 = max(buy1, -p)
        sell1 = max(sell1, buy1 + p)
        buy2 = max(buy2, sell1 - p)
        sell2 = max(sell2, buy2 + p)
    }
    return sell2
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
        var prices: [Int] = []
        for _ in 0..<n {
            prices.append(Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0)
            idx += 1
        }
        out.append(String(maxProfit(prices)))
    }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
