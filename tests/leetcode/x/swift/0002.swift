import Foundation

func addLists(_ a: [Int], _ b: [Int]) -> [Int] {
    var out: [Int] = []
    var i = 0, j = 0, carry = 0
    while i < a.count || j < b.count || carry > 0 {
        var sum = carry
        if i < a.count { sum += a[i]; i += 1 }
        if j < b.count { sum += b[j]; j += 1 }
        out.append(sum % 10)
        carry = sum / 10
    }
    return out
}

func fmt(_ a: [Int]) -> String { "[" + a.map(String.init).joined(separator: ",") + "]" }

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let tokens = data.split { $0 == " " || $0 == "\n" || $0 == "\t" || $0 == "\r" }
if !tokens.isEmpty {
    var idx = 0
    let t = Int(tokens[idx])!
    idx += 1
    var lines: [String] = []
    for _ in 0..<t {
        let n = Int(tokens[idx])!
        idx += 1
        var a: [Int] = []
        for _ in 0..<n { a.append(Int(tokens[idx])!); idx += 1 }
        let m = Int(tokens[idx])!
        idx += 1
        var b: [Int] = []
        for _ in 0..<m { b.append(Int(tokens[idx])!); idx += 1 }
        lines.append(fmt(addLists(a, b)))
    }
    print(lines.joined(separator: "\n"))
}
