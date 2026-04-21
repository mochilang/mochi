import Foundation

func candy(_ ratings: [Int]) -> Int {
    let n = ratings.count
    var candies = Array(repeating: 1, count: n)
    if n >= 2 {
        for i in 1..<n where ratings[i] > ratings[i - 1] {
            candies[i] = candies[i - 1] + 1
        }
        for i in stride(from: n - 2, through: 0, by: -1) where ratings[i] > ratings[i + 1] {
            candies[i] = max(candies[i], candies[i + 1] + 1)
        }
    }
    return candies.reduce(0, +)
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
        var ratings: [Int] = []
        for _ in 0..<n {
            ratings.append(Int(lines[idx]) ?? 0)
            idx += 1
        }
        out.append(String(candy(ratings)))
    }
    print(out.joined(separator: "\n\n"))
}
