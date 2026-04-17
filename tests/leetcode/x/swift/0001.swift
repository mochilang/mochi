import Foundation

func twoSum(_ nums: [Int], _ target: Int) -> (Int, Int) {
    for i in 0..<nums.count {
        if i + 1 < nums.count {
            for j in (i + 1)..<nums.count {
                if nums[i] + nums[j] == target {
                    return (i, j)
                }
            }
        }
    }
    return (0, 0)
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let tokens = data.split { $0 == " " || $0 == "\n" || $0 == "\t" || $0 == "\r" }.compactMap { Int($0) }
if !tokens.isEmpty {
    var idx = 0
    let t = tokens[idx]
    idx += 1
    var out: [String] = []
    for _ in 0..<t {
        let n = tokens[idx]
        let target = tokens[idx + 1]
        idx += 2
        let nums = Array(tokens[idx..<(idx + n)])
        idx += n
        let ans = twoSum(nums, target)
        out.append("\(ans.0) \(ans.1)")
    }
    print(out.joined(separator: "\n"))
}
