import Foundation

func isValid(_ s: String) -> Bool {
    var stack: [Character] = []
    for ch in s {
        if ch == "(" || ch == "[" || ch == "{" {
            stack.append(ch)
        } else {
            if stack.isEmpty { return false }
            let open = stack.removeLast()
            if (ch == ")" && open != "(") || (ch == "]" && open != "[") || (ch == "}" && open != "{") {
                return false
            }
        }
    }
    return stack.isEmpty
}

let data = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8) ?? ""
let tokens = data.split { $0 == " " || $0 == "\n" || $0 == "\t" || $0 == "\r" }
if !tokens.isEmpty {
    let t = Int(tokens[0])!
    let out = (0..<t).map { i in isValid(String(tokens[i + 1])) ? "true" : "false" }
    print(out.joined(separator: "\n"))
}
