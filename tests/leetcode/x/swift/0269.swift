import Foundation

func solve(_ words: [String]) -> String {
  let chars = Set(words.joined()).sorted()
  var adj: [Character: Set<Character>] = [:]
  var indeg: [Character: Int] = [:]
  for c in chars {
    adj[c] = []
    indeg[c] = 0
  }
  for i in 0..<(words.count - 1) {
    let a = Array(words[i])
    let b = Array(words[i + 1])
    let m = min(a.count, b.count)
    if String(a.prefix(m)) == String(b.prefix(m)) && a.count > b.count { return "" }
    for j in 0..<m {
      if a[j] != b[j] {
        if !(adj[a[j]]!.contains(b[j])) {
          adj[a[j]]!.insert(b[j])
          indeg[b[j], default: 0] += 1
        }
        break
      }
    }
  }
  var zeros = chars.filter { indeg[$0] == 0 }
  var out = ""
  while !zeros.isEmpty {
    let c = zeros.removeFirst()
    out.append(c)
    for nei in adj[c]!.sorted() {
      indeg[nei]! -= 1
      if indeg[nei] == 0 {
        zeros.append(nei)
        zeros.sort()
      }
    }
  }
  return out.count == chars.count ? out : ""
}

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)?
  .split(separator: "\n", omittingEmptySubsequences: false).map(String.init) ?? []
if !lines.isEmpty, let t = Int(lines[0].trimmingCharacters(in: .whitespacesAndNewlines)) {
  var out: [String] = []
  var idx = 1
  for _ in 0..<t {
    let n = Int(lines[idx].trimmingCharacters(in: .whitespacesAndNewlines))!
    idx += 1
    out.append(solve(lines[idx..<idx + n].map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }))
    idx += n
  }
  print(out.joined(separator: "\n"), terminator: "")
}
