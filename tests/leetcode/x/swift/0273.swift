import Foundation

let less20 = ["", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine",
              "Ten", "Eleven", "Twelve", "Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen"]
let tens = ["", "", "Twenty", "Thirty", "Forty", "Fifty", "Sixty", "Seventy", "Eighty", "Ninety"]
let thousands = ["", "Thousand", "Million", "Billion"]

func helper(_ n: Int) -> String {
  if n == 0 { return "" }
  if n < 20 { return less20[n] }
  if n < 100 { return tens[n / 10] + (n % 10 == 0 ? "" : " " + helper(n % 10)) }
  return less20[n / 100] + " Hundred" + (n % 100 == 0 ? "" : " " + helper(n % 100))
}

func solve(_ num0: Int) -> String {
  if num0 == 0 { return "Zero" }
  var num = num0
  var parts: [String] = []
  var idx = 0
  while num > 0 {
    let chunk = num % 1000
    if chunk != 0 {
      var words = helper(chunk)
      if !thousands[idx].isEmpty { words += " " + thousands[idx] }
      parts.insert(words, at: 0)
    }
    num /= 1000
    idx += 1
  }
  return parts.joined(separator: " ")
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map(String.init) ?? []
if !lines.isEmpty, let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) {
  var out: [String] = []
  for i in 0..<t {
    out.append(solve(Int(lines[i + 1].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
