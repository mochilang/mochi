// Generated by Mochi transpiler v0.10.50 on 2025-07-31 08:16:36 GMT+7
import Foundation

func _p(_ v: Any?) -> String {
    if let val = v { return String(describing: val) }
    return "<nil>"
}

var _nowSeed = 0
var _nowSeeded = false
func _now() -> Int {
    if !_nowSeeded {
        if let s = ProcessInfo.processInfo.environment["MOCHI_NOW_SEED"], let v = Int(s) {
            _nowSeed = v
            _nowSeeded = true
        }
    }
    if _nowSeeded {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        return _nowSeed
    }
    return Int(Date().timeIntervalSince1970 * 1_000_000_000)
}
func _mem() -> Int {
    if let status = try? String(contentsOfFile: "/proc/self/status") {
        for line in status.split(separator: "\n") {
            if line.hasPrefix("VmRSS:") {
                let parts = line.split(whereSeparator: { $0 == " " || $0 == "\t" })
                if parts.count >= 2, let kb = Int(parts[1]) {
                    return kb * 1024
                }
            }
        }
    }
    return 0
}
func _append<T>(_ xs: [T], _ v: T) -> [T] {
    var out = xs
    out.append(v)
    return out
}
do {
    let _benchMemStart = _mem()
    let _benchStart = _now()
    let width = 60
    let height = (((width as! Double) * 0.86602540378) as! Int)
    let iterations = 5000
    var grid: [[String]] = ([] as! [[String]])
    var y: Int = 0
    while (y < height) {
        var line: [String] = ([] as! [String])
        var x: Int = 0
        while (x < width) {
            line = (_append(line, " ") as! [String])
            x = ((x + 1) as! Int)
        }
        grid = (_append(grid, line) as! [[String]])
        y = ((y + 1) as! Int)
    }
    func randInt(_ s: Int, _ n: Int) -> [Int] {
        let next = (Int(((s * 1664525) + 1013904223)) % 2147483647)
        return ([(next as! Any), ((next % n) as! Any)] as! [Int])
    }
    var seed: Int = 1
    let vertices: [[Int]] = ([(([(0 as! Any), ((height - 1) as! Any)] as! [Int]) as! Any), (([((width - 1) as! Any), ((height - 1) as! Any)] as! [Int]) as! Any), (([(((width / 2) as! Int) as! Any), (0 as! Any)] as! [Int]) as! Any)] as! [[Int]])
    var px: Int = ((width / 2) as! Int)
    var py: Int = ((height / 2) as! Int)
    var i: Int = 0
    while (i < iterations) {
        var r: [Int] = (randInt((seed as! Int), 3) as! [Int])
        seed = (r[0] as! Int)
        let idx = (r[1] as! Int)
        let v: [Int] = (vertices[idx] as! [Int])
        px = ((Int((px + (v[0] as! Int))) / 2) as! Int)
        py = ((Int((py + (v[1] as! Int))) / 2) as! Int)
        if ((((px >= 0) && (px < width)) && (py >= 0)) && (py < height)) {
            grid[py][px] = "*"
        }
        i = ((i + 1) as! Int)
    }
    y = 0
    while (y < height) {
        var line: String = ""
        var x: Int = 0
        while (x < width) {
            line = ((line + (grid[y][x] as! String)) as! String)
            x = ((x + 1) as! Int)
        }
        print(_p(line))
        y = ((y + 1) as! Int)
    }
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
