import Foundation
let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") } ?? []
if !lines.isEmpty, !lines[0].trimmingCharacters(in: .whitespacesAndNewlines).isEmpty {
    var idx = 0
    let t = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0; idx += 1
    var out: [String] = []
    for _ in 0..<t {
        let n = idx < lines.count ? (Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0) : 0; idx += 1
        var arr: [Int] = []
        for _ in 0..<n { arr.append(idx < lines.count ? (Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 0) : 0); idx += 1 }
        let k = idx < lines.count ? (Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines)) ?? 1) : 1; idx += 1
        var i = 0
        while i + k <= arr.count {
            arr[i..<i+k].reverse()
            i += k
        }
        out.append("[" + arr.map(String.init).joined(separator: ",") + "]")
    }
    print(out.joined(separator: "\n"), terminator: "")
}
