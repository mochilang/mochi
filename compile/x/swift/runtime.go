package swiftcode

import "sort"

const (
	helperAvg = `func _avg<T: BinaryInteger>(_ arr: [T]) -> Double {
    if arr.isEmpty { return 0 }
    var sum = 0.0
    for v in arr { sum += Double(v) }
    return sum / Double(arr.count)
}
func _avg<T: BinaryFloatingPoint>(_ arr: [T]) -> Double {
    if arr.isEmpty { return 0 }
    var sum = 0.0
    for v in arr { sum += Double(v) }
    return sum / Double(arr.count)
}
`
	helperIndexString = `func _indexString(_ s: String, _ i: Int) -> String {
    var idx = i
    let chars = Array(s)
    if idx < 0 { idx += chars.count }
    if idx < 0 || idx >= chars.count { fatalError("index out of range") }
    return String(chars[idx])
}
`
	helperIndex = `func _index<T>(_ arr: [T], _ i: Int) -> T {
    var idx = i
    let n = arr.count
    if idx < 0 { idx += n }
    if idx < 0 || idx >= n { fatalError("index out of range") }
    return arr[idx]
}
`
	helperSlice = `func _slice<T>(_ arr: [T], _ i: Int, _ j: Int) -> [T] {
    var start = i
    var end = j
    let n = arr.count
    if start < 0 { start += n }
    if end < 0 { end += n }
    if start < 0 { start = 0 }
    if end > n { end = n }
    if end < start { end = start }
    return Array(arr[start..<end])
}
`
	helperSliceString = `func _sliceString(_ s: String, _ i: Int, _ j: Int) -> String {
    var start = i
    var end = j
    let chars = Array(s)
    let n = chars.count
    if start < 0 { start += n }
    if end < 0 { end += n }
    if start < 0 { start = 0 }
    if end > n { end = n }
    if end < start { end = start }
    return String(chars[start..<end])
        }
`
	helperSum = `func _sum<T: BinaryInteger>(_ arr: [T]) -> Double {
    var sum = 0.0
    for v in arr { sum += Double(v) }
    return sum
}
func _sum<T: BinaryFloatingPoint>(_ arr: [T]) -> Double {
    var sum = 0.0
    for v in arr { sum += Double(v) }
    return sum
}
`
	helperMin = `func _min(_ v: Any) -> Any {
    var list: [Any]? = nil
    if let g = v as? _Group { list = g.Items }
    else if let arr = v as? [Any] { list = arr }
    else if let arr = v as? [Int] { return arr.min() ?? 0 }
    else if let arr = v as? [Double] { return arr.min() ?? 0.0 }
    else if let arr = v as? [String] { return arr.min() ?? "" }
    guard let items = list else { fatalError("min() expects list or group") }
    if items.isEmpty { return 0 }
    if let s = items[0] as? String {
        var m = s
        for it in items.dropFirst() {
            if let v = it as? String, v < m { m = v }
        }
        return m
    }
    func toDouble(_ v: Any) -> Double {
        if let i = v as? Int { return Double(i) }
        if let d = v as? Double { return d }
        if let f = v as? Float { return Double(f) }
        if let i = v as? Int64 { return Double(i) }
        return 0
    }
    var m = toDouble(items[0])
    var isFloat = items[0] is Double || items[0] is Float
    for it in items.dropFirst() {
        if it is Double || it is Float { isFloat = true }
        let d = toDouble(it)
        if d < m { m = d }
    }
        return isFloat ? m : Int(m)
}
`
	helperMax = `func _max(_ v: Any) -> Any {
    var list: [Any]? = nil
    if let g = v as? _Group { list = g.Items }
    else if let arr = v as? [Any] { list = arr }
    else if let arr = v as? [Int] { return arr.max() ?? 0 }
    else if let arr = v as? [Double] { return arr.max() ?? 0.0 }
    else if let arr = v as? [String] { return arr.max() ?? "" }
    guard let items = list else { fatalError("max() expects list or group") }
    if items.isEmpty { return 0 }
    if let s = items[0] as? String {
        var m = s
        for it in items.dropFirst() {
            if let v = it as? String, v > m { m = v }
        }
        return m
    }
    func toDouble(_ v: Any) -> Double {
        if let i = v as? Int { return Double(i) }
        if let d = v as? Double { return d }
        if let f = v as? Float { return Double(f) }
        if let i = v as? Int64 { return Double(i) }
        return 0
    }
    var m = toDouble(items[0])
    var isFloat = items[0] is Double || items[0] is Float
    for it in items.dropFirst() {
        if it is Double || it is Float { isFloat = true }
        let d = toDouble(it)
        if d > m { m = d }
    }
    return isFloat ? m : Int(m)
}`
	helperUnionAll = `func _union_all<T>(_ a: [T], _ b: [T]) -> [T] {
    var res = a
    res.append(contentsOf: b)
    return res
}`
	helperUnion = `func _union<T: Equatable>(_ a: [T], _ b: [T]) -> [T] {
    var res: [T] = a
    for it in b { if !res.contains(it) { res.append(it) } }
    return res
}`
	helperExcept = `func _except<T: Equatable>(_ a: [T], _ b: [T]) -> [T] {
    var res: [T] = []
    for it in a { if !b.contains(it) { res.append(it) } }
    return res
}`
	helperIntersect = `func _intersect<T: Equatable>(_ a: [T], _ b: [T]) -> [T] {
    var res: [T] = []
    for it in a { if b.contains(it) && !res.contains(it) { res.append(it) } }
    return res
}`
	helperConcat = `func _concat<T>(_ a: [T], _ b: [T]) -> [T] {
    var res: [T] = []
    res.reserveCapacity(a.count + b.count)
    res.append(contentsOf: a)
    res.append(contentsOf: b)
    return res
}`
	helperAppend = `func _append<T>(_ arr: [T], _ v: T) -> [T] {
    var out = arr
    out.append(v)
    return out
}`
	helperValues = `func _values(_ m: [String: Any]) -> [Any] {
    return Array(m.values)
}`
	helperExists = `func _exists(_ v: Any) -> Bool {
    if let g = v as? _Group { return !g.Items.isEmpty }
    if let arr = v as? [Any] { return !arr.isEmpty }
    if let arr = v as? [Int] { return !arr.isEmpty }
    if let arr = v as? [Double] { return !arr.isEmpty }
    if let arr = v as? [String] { return !arr.isEmpty }
    if let m = v as? [String: Any] { return !m.isEmpty }
    if let s = v as? String { return !s.isEmpty }
    return false
}`
	helperCast = `func _cast<T: Decodable>(_ type: T.Type, _ v: Any) -> T {
    if let tv = v as? T { return tv }
    if let data = try? JSONSerialization.data(withJSONObject: v),
       let obj = try? JSONDecoder().decode(T.self, from: data) {
        return obj
    }
    fatalError("cast failed")
}`
	helperToMapSlice = `func _toMapSlice(_ v: Any) -> [[String: Any]] {
    if let rows = v as? [[String: Any]] { return rows }
    var arr: [Any] = []
    if let a = v as? [Any] { arr = a } else { arr = [v] }
    var out: [[String: Any]] = []
    for item in arr {
        if let m = item as? [String: Any] { out.append(m); continue }
        let mirror = Mirror(reflecting: item)
        if mirror.displayStyle == .struct || mirror.displayStyle == .class {
            var m: [String: Any] = [:]
            for child in mirror.children {
                if let k = child.label { m[k] = child.value }
            }
            out.append(m)
        }
    }
    return out
}`
	helperLoad = `func _readInput(_ path: String?) -> String {
    if let p = path, !p.isEmpty && p != "-" {
        return (try? String(contentsOfFile: p)) ?? ""
    }
    let data = FileHandle.standardInput.readDataToEndOfFile()
    return String(data: data, encoding: .utf8) ?? ""
}
func _parseCSV(_ text: String, _ header: Bool, _ delim: Character) -> [[String: Any]] {
    let lines = text.split(whereSeparator: { $0 == "\n" || $0 == "\r" })
    if lines.isEmpty { return [] }
    var headers: [String] = []
    var start = 0
    if header {
        headers = lines[0].split(separator: delim).map { String($0) }
        start = 1
    } else {
        headers = lines[0].split(separator: delim).enumerated().map { "c" + String($0.offset) }
    }
    var out: [[String: Any]] = []
    for i in start..<lines.count {
        let parts = lines[i].split(separator: delim)
        var row: [String: Any] = [:]
        for j in 0..<headers.count {
            let val = j < parts.count ? String(parts[j]) : ""
            if let iv = Int(val) { row[headers[j]] = iv } else if let dv = Double(val) { row[headers[j]] = dv } else { row[headers[j]] = val }
        }
        out.append(row)
    }
    return out
}
func _load(_ path: String?, _ opts: [String: Any]?) -> [[String: Any]] {
    let format = (opts?["format"] as? String) ?? "csv"
    let header = (opts?["header"] as? Bool) ?? true
    var delim: Character = ','
    if let d = opts?["delimiter"] as? String, !d.isEmpty { delim = d.first! }
    let text = _readInput(path)
    switch format {
    case "jsonl":
        return text.split(separator: "\n").filter { !$0.isEmpty }.compactMap { line in
            if let data = line.data(using: .utf8), let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any] { return obj }
            return nil
        }
    case "json":
        if let data = text.data(using: .utf8) {
            if let arr = try? JSONSerialization.jsonObject(with: data) as? [[String: Any]] { return arr }
            if let obj = try? JSONSerialization.jsonObject(with: data) as? [String: Any] { return [obj] }
        }
        return []
    case "tsv":
        delim = '\t'
        fallthrough
    default:
        return _parseCSV(text, header, delim)
    }
}
`
	helperSave = `func _writeOutput(_ path: String?, _ text: String) {
    if let p = path, !p.isEmpty && p != "-" {
        try? text.write(toFile: p, atomically: true, encoding: .utf8)
    } else {
        if let data = text.data(using: .utf8) { FileHandle.standardOutput.write(data) }
    }
}
func _save(_ rows: [[String: Any]], _ path: String?, _ opts: [String: Any]?) {
    let format = (opts?["format"] as? String) ?? "csv"
    let header = (opts?["header"] as? Bool) ?? false
    var delim: Character = ','
    if let d = opts?["delimiter"] as? String, !d.isEmpty { delim = d.first! }
    var text = ""
    switch format {
    case "jsonl":
        for r in rows { if let d = try? JSONSerialization.data(withJSONObject: r), let s = String(data: d, encoding: .utf8) { text += s + "\n" } }
    case "json":
        let obj: Any = rows.count == 1 ? rows[0] : rows
        if let d = try? JSONSerialization.data(withJSONObject: obj), let s = String(data: d, encoding: .utf8) { text = s }
    case "tsv":
        delim = '\t'
        fallthrough
    default:
        let headers = rows.isEmpty ? [] : rows[0].keys.sorted()
        var lines: [String] = []
        if header && !headers.isEmpty { lines.append(headers.joined(separator: String(delim))) }
        for r in rows {
            let rec = headers.map { h in String(describing: r[h] ?? "") }
            lines.append(rec.joined(separator: String(delim)))
        }
        text = lines.joined(separator: "\n") + "\n"
    }
    _writeOutput(path, text)
}
`
	helperFetch = `func _fetch(_ urlStr: String, _ opts: [String: Any]?) -> Any {
    var comps = URLComponents(string: urlStr)
    if let q = opts?["query"] as? [String: Any] {
        var items = comps?.queryItems ?? []
        for (k, v) in q {
            items.append(URLQueryItem(name: k, value: String(describing: v)))
        }
        comps?.queryItems = items
    }
    guard let url = comps?.url else { return [:] }
    var req = URLRequest(url: url)
    req.httpMethod = (opts?["method"] as? String) ?? "GET"
    if let b = opts?["body"], let d = try? JSONSerialization.data(withJSONObject: b) {
        req.httpBody = d
    }
    if let hs = opts?["headers"] as? [String: Any] {
        for (k, v) in hs {
            if let s = v as? String { req.setValue(s, forHTTPHeaderField: k) }
        }
    }
    let timeout: TimeInterval
    if let t = opts?["timeout"] {
        if let v = t as? Double { timeout = v }
        else if let v = t as? Float { timeout = TimeInterval(v) }
        else if let v = t as? Int { timeout = TimeInterval(v) }
        else { timeout = 60 }
    } else {
        timeout = 60
    }
    let config = URLSessionConfiguration.default
    config.timeoutIntervalForRequest = timeout
    let session = URLSession(configuration: config)
    var out: Any = [:]
    let sem = DispatchSemaphore(value: 0)
    let task = session.dataTask(with: req) { data, _, _ in
        if let data = data {
            if let obj = try? JSONSerialization.jsonObject(with: data) {
                out = obj
            } else {
                out = String(data: data, encoding: .utf8) ?? ""
            }
        }
        sem.signal()
    }
    task.resume()
    sem.wait()
    return out
}
`
	helperGroup = `class _Group {
    var key: Any
    var Items: [Any] = []
    init(_ k: Any) { self.key = k }
}
`
	helperGroupBy = `func _group_by(_ src: [Any], _ keyfn: (Any) -> Any) -> [_Group] {
    func keyStr(_ v: Any) -> String {
        if let data = try? JSONSerialization.data(withJSONObject: v, options: [.sortedKeys]),
           let s = String(data: data, encoding: .utf8) {
            return s
        }
        return String(describing: v)
    }
    var groups: [String: _Group] = [:]
    var order: [String] = []
    for it in src {
        let key = keyfn(it)
        let ks = keyStr(key)
        if groups[ks] == nil {
            groups[ks] = _Group(key)
            order.append(ks)
        }
        groups[ks]!.Items.append(it)
    }
    return order.compactMap { groups[$0] }
}
`
	helperJSON = `func _json(_ v: Any) {
    if let d = try? JSONSerialization.data(withJSONObject: v, options: []), let s = String(data: d, encoding: .utf8) {
        print(s)
    }
}
`
)

var helperMap = map[string]string{
	"_avg":         helperAvg,
	"_sum":         helperSum,
	"_min":         helperMin,
	"_max":         helperMax,
	"_indexString": helperIndexString,
	"_index":       helperIndex,
	"_slice":       helperSlice,
	"_sliceString": helperSliceString,
	"_concat":      helperConcat,
	"_append":      helperAppend,
	"_values":      helperValues,
	"_exists":      helperExists,
	"_union_all":   helperUnionAll,
	"_union":       helperUnion,
	"_except":      helperExcept,
	"_intersect":   helperIntersect,
	"_cast":        helperCast,
	"_toMapSlice":  helperToMapSlice,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_fetch":       helperFetch,
	"_Group":       helperGroup,
	"_group_by":    helperGroupBy,
	"_json":        helperJSON,
}

func (c *Compiler) use(name string) { c.helpers[name] = true }

func (c *Compiler) emitRuntime() {
	if len(c.helpers) == 0 {
		return
	}
	names := make([]string, 0, len(c.helpers))
	for n := range c.helpers {
		names = append(names, n)
	}
	sort.Strings(names)
	for _, n := range names {
		c.buf.WriteString(helperMap[n])
		c.buf.WriteByte('\n')
	}
}
