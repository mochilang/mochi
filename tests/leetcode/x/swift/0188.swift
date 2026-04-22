import Foundation

func solve(_ k: Int, _ prices: [Int]) -> Int {
  let n = prices.count
  if k >= n / 2 {
    var best = 0
    if n > 1 {
      for i in 1..<n where prices[i] > prices[i - 1] {
        best += prices[i] - prices[i - 1]
      }
    }
    return best
  }
  let negInf = Int.min / 4
  var buy = Array(repeating: negInf, count: k + 1)
  var sell = Array(repeating: 0, count: k + 1)
  for price in prices {
    for t in 1...k {
      buy[t] = max(buy[t], sell[t - 1] - price)
      sell[t] = max(sell[t], buy[t] + price)
    }
  }
  return sell[k]
}

let toks = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split { $0 == " " || $0 == "\n" || $0 == "\r" || $0 == "\t" }.map(String.init) ?? []
if !toks.isEmpty {
  var idx = 0
  let t = Int(toks[idx]) ?? 0
  idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let k = Int(toks[idx]) ?? 0
    let n = Int(toks[idx + 1]) ?? 0
    idx += 2
    var prices: [Int] = []
    for _ in 0..<n { prices.append(Int(toks[idx]) ?? 0); idx += 1 }
    out.append(String(solve(k, prices)))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
