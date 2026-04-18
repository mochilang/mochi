import Foundation

func maxProfit(_ prices: [Int]) -> Int {
    var best = 0
    for i in 1..<prices.count {
        if prices[i] > prices[i - 1] {
            best += prices[i] - prices[i - 1]
        }
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
        var prices: [Int] = []
        for _ in 0..<n {
            prices.append(Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0)
            idx += 1
        }
        out.append(String(maxProfit(prices)))
    }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
