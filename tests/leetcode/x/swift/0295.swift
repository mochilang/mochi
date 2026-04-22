import Foundation

final class MedianFinder {
    private var data: [Int] = []

    func addNum(_ num: Int) {
        var lo = 0
        var hi = data.count
        while lo < hi {
            let mid = (lo + hi) / 2
            if data[mid] < num {
                lo = mid + 1
            } else {
                hi = mid
            }
        }
        data.insert(num, at: lo)
    }

    func findMedian() -> Double {
        let n = data.count
        if n % 2 == 1 {
            return Double(data[n / 2])
        }
        return Double(data[n / 2 - 1] + data[n / 2]) / 2.0
    }
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)!
    .split(whereSeparator: \.isNewline)
    .map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
    .filter { !$0.isEmpty }

if !lines.isEmpty {
    let t = Int(lines[0])!
    var idx = 1
    var blocks: [String] = []
    for _ in 0..<t {
        let m = Int(lines[idx])!
        idx += 1
        let mf = MedianFinder()
        var out: [String] = []
        for _ in 0..<m {
            let parts = lines[idx].split(separator: " ").map(String.init)
            idx += 1
            if parts[0] == "addNum" {
                mf.addNum(Int(parts[1])!)
            } else {
                out.append(String(format: "%.1f", mf.findMedian()))
            }
        }
        blocks.append(out.joined(separator: "\n"))
    }
    print(blocks.joined(separator: "\n\n"), terminator: "")
}
