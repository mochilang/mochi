import Foundation
func trap(_ h: [Int]) -> Int {
  var left = 0, right = h.count - 1, leftMax = 0, rightMax = 0, water = 0
  while left <= right {
    if leftMax <= rightMax {
      if h[left] < leftMax { water += leftMax - h[left] } else { leftMax = h[left] }
      left += 1
    } else {
      if h[right] < rightMax { water += rightMax - h[right] } else { rightMax = h[right] }
      right -= 1
    }
  }
  return water
}
let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "").trimmingCharacters(in: .whitespacesAndNewlines) } ?? []
if !lines.isEmpty && !lines[0].isEmpty {
  var idx = 0
  let t = Int(lines[idx]) ?? 0; idx += 1
  var out: [String] = []
  for _ in 0..<t {
    let n = Int(lines[idx]) ?? 0; idx += 1
    var arr: [Int] = []
    for _ in 0..<n { arr.append(Int(lines[idx]) ?? 0); idx += 1 }
    out.append(String(trap(arr)))
  }
  print(out.joined(separator: "\n"), terminator: "")
}
