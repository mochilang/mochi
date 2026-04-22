import Foundation

func minTotalDistance(_ grid: [[Int]]) -> Int {
    var rows: [Int] = []
    var cols: [Int] = []
    for i in 0..<grid.count {
        for j in 0..<grid[i].count where grid[i][j] == 1 {
            rows.append(i)
        }
    }
    for j in 0..<grid[0].count {
        for i in 0..<grid.count where grid[i][j] == 1 {
            cols.append(j)
        }
    }
    let mr = rows[rows.count / 2]
    let mc = cols[cols.count / 2]
    return rows.reduce(0) { $0 + abs($1 - mr) } + cols.reduce(0) { $0 + abs($1 - mc) }
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)!
    .split { $0 == " " || $0 == "\n" || $0 == "\t" || $0 == "\r" }
    .compactMap { Int($0) }

if !data.isEmpty {
    var idx = 0
    let t = data[idx]
    idx += 1
    var out: [String] = []
    for _ in 0..<t {
        let r = data[idx]
        let c = data[idx + 1]
        idx += 2
        var grid = Array(repeating: Array(repeating: 0, count: c), count: r)
        for i in 0..<r {
            for j in 0..<c {
                grid[i][j] = data[idx]
                idx += 1
            }
        }
        out.append(String(minTotalDistance(grid)))
    }
    print(out.joined(separator: "\n\n"), terminator: "")
}
