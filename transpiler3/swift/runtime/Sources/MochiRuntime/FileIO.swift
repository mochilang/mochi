import Foundation

public func mochiReadFile(_ path: String) -> String {
    (try? String(contentsOfFile: path, encoding: .utf8)) ?? ""
}

public func mochiWriteFile(_ path: String, _ content: String) {
    try? content.write(toFile: path, atomically: true, encoding: .utf8)
}

public func mochiAppendFile(_ path: String, _ content: String) {
    if let data = content.data(using: .utf8) {
        if FileManager.default.fileExists(atPath: path) {
            if let fh = FileHandle(forWritingAtPath: path) {
                fh.seekToEndOfFile()
                fh.write(data)
                fh.closeFile()
            }
        } else {
            try? content.write(toFile: path, atomically: true, encoding: .utf8)
        }
    }
}

public func mochiLines(_ path: String) -> [String] {
    guard let content = try? String(contentsOfFile: path, encoding: .utf8) else { return [] }
    let lines = content.components(separatedBy: "\n")
    // Remove trailing empty line from files ending with \n
    if lines.last == "" {
        return Array(lines.dropLast())
    }
    return lines
}
