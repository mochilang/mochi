import Foundation

func myAtoi(_ s: String) -> Int {
    let chars = Array(s)
    var i = 0
    while i < chars.count && chars[i] == " " { i += 1 }
    var sign = 1
    if i < chars.count && (chars[i] == "+" || chars[i] == "-") {
        if chars[i] == "-" { sign = -1 }
        i += 1
    }
    var ans = 0
    let limit = sign > 0 ? 7 : 8
    while i < chars.count, let digit = chars[i].wholeNumberValue {
        if ans > 214748364 || (ans == 214748364 && digit > limit) { return sign > 0 ? Int(Int32.max) : Int(Int32.min) }
        ans = ans * 10 + digit
        i += 1
    }
    return sign * ans
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var out: [String] = []
    for i in 0..<t { out.append(String(myAtoi(i + 1 < lines.count ? lines[i + 1] : ""))) }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
