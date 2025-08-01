// Generated by Mochi transpiler v0.10.42 on 2025-07-28 10:32:24 GMT+7
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
    let INF = 1000000000
    var graph: [String: [String: Int]] = ([:] as! [String: [String: Int]])
    func addEdge(_ u: String, _ v: String, _ w: Int) {
        if (!(graph[u] != nil)) {
            graph[u] = ([:] as! [String: Int])
        }
        graph[u]![v] = Int(w)
        if (!(graph[v] != nil)) {
            graph[v] = ([:] as! [String: Int])
        }
    }
    func removeAt(_ xs: [String], _ idx: Int) -> [String] {
        var out: [String] = ([] as! [String])
        var i: Int = 0
        for x in xs {
            if (i != idx) {
                out = (_append(out, x) as! [String])
            }
            i = Int((i + 1))
        }
        return (out as! [String])
    }
    func dijkstra(_ source: String) -> [String: Any] {
        var dist: [String: Int] = ([:] as! [String: Int])
        var prev: [String: String] = ([:] as! [String: String])
        for v in graph.keys.sorted() {
            dist[v] = Int(INF)
            prev[v] = ""
        }
        dist[source] = 0
        var q: [String] = ([] as! [String])
        for v in graph.keys.sorted() {
            q = (_append(q, v) as! [String])
        }
        while (Int(((q).count)) > 0) {
            var bestIdx: Int = 0
            var u: String = String(describing: q[0])
            var i: Int = 1
            while (i < Int(((q).count))) {
                let v = String(describing: q[i])
                if (dist[v]! < dist[u]!) {
                    u = String(describing: v)
                    bestIdx = Int(i)
                }
                i = Int((i + 1))
            }
            q = (removeAt((q as! [String]), Int(bestIdx)) as! [String])
            for v in graph[u]!.keys.sorted() {
                let alt = (dist[u]! + graph[u]![v]!)
                if (alt < dist[v]!) {
                    dist[v] = Int(alt)
                    prev[v] = String(describing: u)
                }
            }
        }
        return (["dist": dist, "prev": prev] as! [String: Any])
    }
    func path(_ prev: [String: String], _ v: String) -> String {
        var s: String = v
        var cur: String = v
        while (prev[cur]! != "") {
            cur = String(describing: prev[cur]!)
            s = String(describing: (cur + s))
        }
        return String(describing: s)
    }
    func main() {
        _ = addEdge("a", "b", 7)
        _ = addEdge("a", "c", 9)
        _ = addEdge("a", "f", 14)
        _ = addEdge("b", "c", 10)
        _ = addEdge("b", "d", 15)
        _ = addEdge("c", "d", 11)
        _ = addEdge("c", "f", 2)
        _ = addEdge("d", "e", 6)
        _ = addEdge("e", "f", 9)
        let res = (dijkstra("a") as! [String: Any])
        let dist = (res["dist"]! as! [String: Int])
        let prev = (res["prev"]! as! [String: String])
        print(_p(((("Distance to e: " + _p(dist["e"]!)) + ", Path: ") + String(describing: path((prev as! [String: String]), "e")))))
        print(_p(((("Distance to f: " + _p(dist["f"]!)) + ", Path: ") + String(describing: path((prev as! [String: String]), "f")))))
    }
    _ = main()
    let _benchEnd = _now()
    let _benchMemEnd = _mem()
    print("{\n  \"duration_us\": \((_benchEnd - _benchStart) / 1000),\n  \"memory_bytes\": \(_benchMemEnd - _benchMemStart),\n  \"name\": \"main\"\n}")
}
