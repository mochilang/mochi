import Foundation

func solve(_ values: [Int], _ target: Double, _ k: Int) -> [Int] {
  var right = 0
  while right < values.count && Double(values[right]) < target { right += 1 }
  var left = right - 1
  var ans: [Int] = []
  while ans.count < k {
    if left < 0 {
      ans.append(values[right]); right += 1
    } else if right >= values.count {
      ans.append(values[left]); left -= 1
    } else if abs(Double(values[left]) - target) <= abs(Double(values[right]) - target) {
      ans.append(values[left]); left -= 1
    } else {
      ans.append(values[right]); right += 1
    }
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
    var values: [Int] = []
    for _ in 0..<n {
      values.append(Int(toks[idx])!)
      idx += 1
    }
    let target = Double(toks[idx])!
    idx += 1
    let k = Int(toks[idx])!
    idx += 1
    let ans = solve(values, target, k)
    blocks.append(([String(ans.count)] + ans.map(String.init)).joined(separator: "\n"))
  }
  print(blocks.joined(separator: "\n\n"), terminator: "")
}
