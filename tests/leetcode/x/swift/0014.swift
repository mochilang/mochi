import Foundation

func lcp(_ strs: [String]) -> String {
    var prefix = strs[0]
    while !strs.allSatisfy({ $0.hasPrefix(prefix) }) {
        prefix.removeLast()
    }
    return prefix
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let tokens = data.split { $0 == " " || $0 == "\n" || $0 == "\t" || $0 == "\r" }
if !tokens.isEmpty {
    var idx = 0
    let t = Int(tokens[idx])!; idx += 1
    var out: [String] = []
    for _ in 0..<t {
        let n = Int(tokens[idx])!; idx += 1
        var strs: [String] = []
        for _ in 0..<n { strs.append(String(tokens[idx])); idx += 1 }
        out.append("\"\(lcp(strs))\"")
    }
    print(out.joined(separator: "\n"))
}
