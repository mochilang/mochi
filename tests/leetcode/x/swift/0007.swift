import Foundation

func reverseInt(_ x0: Int) -> Int {
    var x = x0
    var ans = 0
    while x != 0 {
        let digit = x % 10
        x /= 10
        if ans > Int32.max / 10 || (ans == Int32.max / 10 && digit > 7) { return 0 }
        if ans < Int32.min / 10 || (ans == Int32.min / 10 && digit < -8) { return 0 }
        ans = ans * 10 + digit
    }
    return ans
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var out: [String] = []
    for i in 0..<t {
        let x = i + 1 < lines.count ? (Int(lines[i + 1].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0) : 0
        out.append(String(reverseInt(x)))
    }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
