// Generated by Mochi transpiler v0.10.50 on 2025-07-31 00:25:02 GMT+7
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
do {
    let _benchMemStart = _mem()
    let _benchStart = _now()
    struct Point {
        var x: Double
        var y: Double
    }
    struct Line {
        var slope: Double
        var yint: Double
    }
    func createLine(_ a: Point, _ b: Point) -> Line {
        let slope = (Double((b.y - a.y)) / Double((b.x - a.x)))
        let yint = (a.y - (slope * a.x))
        return (Line(slope: slope, yint: yint) as! Line)
    }
    func evalX(_ l: Line, _ x: Double) -> Double {
        return Double(((l.slope * x) + l.yint))
    }
    func intersection(_ l1: Line, _ l2: Line) -> Point {
        if (l1.slope == l2.slope) {
            return (Point(x: 0.0, y: 0.0) as! Point)
        }
        let x = (Double((l2.yint - l1.yint)) / Double((l1.slope - l2.slope)))
        let y = Double(evalX((l1 as! Line), Double(x)))
        return (Point(x: x, y: y) as! Point)
    }
    func main() {
        let l1 = createLine((Point(x: 4.0, y: 0.0) as! Point), (Point(x: 6.0, y: 10.0) as! Point))
        let l2 = createLine((Point(x: 0.0, y: 3.0) as! Point), (Point(x: 10.0, y: 7.0) as! Point))
        let p = intersection((l1 as! Line), (l2 as! Line))
        print(_p((((("{" + _p(p.x)) + " ") + _p(p.y)) + "}")))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
