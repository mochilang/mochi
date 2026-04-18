import Foundation

func firstMissingPositive(_ nums: inout [Int]) -> Int {
    let n = nums.count
    var i = 0
    while i < n {
        let v = nums[i]
        if v >= 1 && v <= n && nums[v - 1] != v {
            nums.swapAt(i, v - 1)
        } else {
            i += 1
        }
    }
    for i in 0..<n {
        if nums[i] != i + 1 { return i + 1 }
    }
    return n + 1
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
    .split(separator: "\n", omittingEmptySubsequences: false)
    .map { String($0).replacingOccurrences(of: "\r", with: "").trimmingCharacters(in: .whitespacesAndNewlines) } ?? []
if !lines.isEmpty && !lines[0].isEmpty {
    var idx = 0
    let t = Int(lines[idx]) ?? 0
    idx += 1
    var out: [String] = []
    for _ in 0..<t {
        let n = Int(lines[idx]) ?? 0
        idx += 1
        var nums: [Int] = []
        for _ in 0..<n { nums.append(Int(lines[idx]) ?? 0); idx += 1 }
        out.append(String(firstMissingPositive(&nums)))
    }
    print(out.joined(separator: "\n"), terminator: "")
}
