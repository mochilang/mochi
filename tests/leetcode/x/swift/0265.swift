import Foundation

func solve(_ costs: [[Int]]) -> Int {
  if costs.isEmpty { return 0 }
  var prev = costs[0]
  if costs.count > 1 {
    for r in 1..<costs.count {
      var min1 = Int.max
      var min2 = Int.max
      var idx1 = -1
      for i in 0..<prev.count {
        if prev[i] < min1 {
          min2 = min1
          min1 = prev[i]
          idx1 = i
        } else if prev[i] < min2 {
          min2 = prev[i]
        }
      }
      var cur = Array(repeating: 0, count: prev.count)
      for i in 0..<prev.count {
        cur[i] = costs[r][i] + (i == idx1 ? min2 : min1)
      }
      prev = cur
    }
  }
  return prev.min()!
}

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx])!
  idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let n = Int(toks[idx])!
    idx += 1
    let k = Int(toks[idx])!
    idx += 1
    var costs: [[Int]] = []
    for _ in 0..<n {
      var row: [Int] = []
      for _ in 0..<k {
        row.append(Int(toks[idx])!)
        idx += 1
      }
      costs.append(row)
    }
    out.append(String(solve(costs)))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
