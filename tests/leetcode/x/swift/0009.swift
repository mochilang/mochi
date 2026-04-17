import Foundation

func isPalindrome(_ x: Int) -> Bool {
    if x < 0 { return false }
    let original = x
    var n = x
    var rev = 0
    while n > 0 {
        rev = rev * 10 + (n % 10)
        n /= 10
    }
    return rev == original
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let tokens = data.split { $0 == " " || $0 == "\n" || $0 == "\t" || $0 == "\r" }
if !tokens.isEmpty {
    let t = Int(tokens[0])!
    let out = (0..<t).map { i in isPalindrome(Int(tokens[i + 1])!) ? "true" : "false" }
    print(out.joined(separator: "\n"))
}
