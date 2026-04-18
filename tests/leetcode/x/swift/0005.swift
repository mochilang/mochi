import Foundation

func expand(_ chars: [Character], _ left0: Int, _ right0: Int) -> (Int, Int) {
    var left = left0
    var right = right0
    while left >= 0 && right < chars.count && chars[left] == chars[right] {
        left -= 1
        right += 1
    }
    return (left + 1, right - left - 1)
}

func longestPalindrome(_ s: String) -> String {
    let chars = Array(s)
    var bestStart = 0
    var bestLen = chars.isEmpty ? 0 : 1
    for i in 0..<chars.count {
        let odd = expand(chars, i, i)
        if odd.1 > bestLen {
            bestStart = odd.0
            bestLen = odd.1
        }
        let even = expand(chars, i, i + 1)
        if even.1 > bestLen {
            bestStart = even.0
            bestLen = even.1
        }
    }
    return String(chars[bestStart..<bestStart + bestLen])
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var out: [String] = []
    for i in 0..<t {
        let s = i + 1 < lines.count ? lines[i + 1] : ""
        out.append(longestPalindrome(s))
    }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
