import Foundation

let lines = String(data: FileHandle.standardInput.readDataToEndOfFile(), encoding: .utf8)!
    .split(whereSeparator: \.isNewline)
    .map { $0.trimmingCharacters(in: .whitespacesAndNewlines) }
    .filter { !$0.isEmpty }

if !lines.isEmpty {
    let t = Int(lines[0])!
    print(lines[1...t].joined(separator: "\n\n"), terminator: "")
}
