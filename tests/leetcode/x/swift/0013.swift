import Foundation

let values: [Character: Int] = ["I": 1, "V": 5, "X": 10, "L": 50, "C": 100, "D": 500, "M": 1000]

func romanToInt(_ s: String) -> Int {
    let chars = Array(s)
    var total = 0
    for i in 0..<chars.count {
        let cur = values[chars[i]]!
        let next = i + 1 < chars.count ? values[chars[i + 1]]! : 0
        total += cur < next ? -cur : cur
    }
    return total
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let tokens = data.split { $0 == " " || $0 == "\n" || $0 == "\t" || $0 == "\r" }
if !tokens.isEmpty {
    let t = Int(tokens[0])!
    let out = (0..<t).map { i in String(romanToInt(String(tokens[i + 1]))) }
    print(out.joined(separator: "\n"))
}
