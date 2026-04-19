import Foundation

func ladders(_ begin: String, _ end: String, _ words: [String]) -> [[String]] {
    let wordSet = Set(words)
    if !wordSet.contains(end) { return [] }
    var parents: [String: [String]] = [:]
    var level: Set<String> = [begin]
    var visited: Set<String> = [begin]
    var found = false
    while !level.isEmpty && !found {
        var next: Set<String> = []
        for word in level.sorted() {
            var arr = Array(word)
            for i in arr.indices {
                let orig = arr[i]
                for scalar in UnicodeScalar("a").value...UnicodeScalar("z").value {
                    let c = Character(UnicodeScalar(scalar)!)
                    if c == orig { continue }
                    arr[i] = c
                    let nw = String(arr)
                    if !wordSet.contains(nw) || visited.contains(nw) { continue }
                    next.insert(nw)
                    parents[nw, default: []].append(word)
                    if nw == end { found = true }
                }
                arr[i] = orig
            }
        }
        visited.formUnion(next)
        level = next
    }
    if !found { return [] }
    var out: [[String]] = []
    var path = [end]
    func dfs(_ word: String) {
        if word == begin {
            out.append(path.reversed())
            return
        }
        for p in (parents[word] ?? []).sorted() {
            path.append(p)
            dfs(p)
            path.removeLast()
        }
    }
    dfs(end)
    return out.sorted { $0.joined(separator: "->") < $1.joined(separator: "->") }
}

func fmt(_ paths: [[String]]) -> String {
    var lines = [String(paths.count)]
    for p in paths { lines.append(p.joined(separator: "->")) }
    return lines.joined(separator: "\n")
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let lines = data.split(separator: "\n", omittingEmptySubsequences: false).map { String($0).replacingOccurrences(of: "\r", with: "") }
if !lines.isEmpty {
    let tc = Int(lines[0]) ?? 0
    var idx = 1
    var out: [String] = []
    for _ in 0..<tc {
        let begin = lines[idx]; idx += 1
        let end = lines[idx]; idx += 1
        let n = Int(lines[idx]) ?? 0; idx += 1
        let words = Array(lines[idx..<idx+n]); idx += n
        out.append(fmt(ladders(begin, end, words)))
    }
    print(out.joined(separator: "\n\n"))
}
