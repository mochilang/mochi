import Foundation

func median(_ a: [Int], _ b: [Int]) -> Double {
    let m = (a + b).sorted()
    if m.count % 2 == 1 { return Double(m[m.count / 2]) }
    return Double(m[m.count / 2 - 1] + m[m.count / 2]) / 2.0
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0
    var idx = 1
    var out: [String] = []
    for _ in 0..<t {
        let n = Int(lines[idx]) ?? 0; idx += 1
        var a: [Int] = []
        for _ in 0..<n { a.append(Int(lines[idx]) ?? 0); idx += 1 }
        let m = Int(lines[idx]) ?? 0; idx += 1
        var b: [Int] = []
        for _ in 0..<m { b.append(Int(lines[idx]) ?? 0); idx += 1 }
        out.append(String(format: "%.1f", median(a,b)))
    }
    FileHandle.standardOutput.write(out.joined(separator: "\n").data(using: .utf8)!)
}
