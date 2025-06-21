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
    guard let url = URL(string: urlStr) else { return [:] }
    if let data = try? Data(contentsOf: url) {
        if let obj = try? JSONSerialization.jsonObject(with: data) { return obj }
        return String(data: data, encoding: .utf8) ?? ""
    }
    return [:]
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
	"_indexString": helperIndexString,
	"_index":       helperIndex,
	"_slice":       helperSlice,
	"_sliceString": helperSliceString,
	"_load":        helperLoad,
	"_save":        helperSave,
	"_fetch":       helperFetch,
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
