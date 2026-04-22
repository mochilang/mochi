import Foundation

func solve(_ nums: [Int], _ k: Int) -> [Int] {
  var dq: [Int] = []
  var ans: [Int] = []
  for i in nums.indices {
    while !dq.isEmpty && dq.first! <= i - k { dq.removeFirst() }
    while !dq.isEmpty && nums[dq.last!] <= nums[i] { dq.removeLast() }
    dq.append(i)
    if i >= k - 1 { ans.append(nums[dq.first!]) }
  }
  return ans
}

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx])!
  idx += 1
  var blocks: [String] = []
  for _ in 0..<t {
    let n = Int(toks[idx])!
    idx += 1
    var nums: [Int] = []
    for _ in 0..<n {
      nums.append(Int(toks[idx])!)
      idx += 1
    }
    let k = Int(toks[idx])!
    idx += 1
    let ans = solve(nums, k)
    blocks.append(([String(ans.count)] + ans.map(String.init)).joined(separator: "\n"))
  }
  print(blocks.joined(separator: "\n\n"), terminator: "")
}
