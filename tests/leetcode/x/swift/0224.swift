import Foundation

func calculate(_ expr: String) -> Int {
  var result = 0
  var number = 0
  var sign = 1
  var stack: [Int] = []
  for ch in expr {
    if ch >= "0" && ch <= "9" {
      number = number * 10 + Int(String(ch))!
    } else if ch == "+" || ch == "-" {
      result += sign * number
      number = 0
      sign = ch == "+" ? 1 : -1
    } else if ch == "(" {
      stack.append(result)
      stack.append(sign)
      result = 0
      number = 0
      sign = 1
    } else if ch == ")" {
      result += sign * number
      number = 0
      let prevSign = stack.removeLast()
      let prevResult = stack.removeLast()
      result = prevResult + prevSign * result
    }
  }
  return result + sign * number
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map(String.init) ?? []
if !lines.isEmpty, let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) {
  var out: [String] = []
  for i in 0..<t {
    out.append(String(calculate(lines[i + 1])))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
