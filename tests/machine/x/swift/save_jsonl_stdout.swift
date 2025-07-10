import Foundation

func _save(_ rows: [[String:Any]], path: String, opts: [String:Any]?) {
    let format = (opts?["format"] as? String) ?? "csv"
    if format == "jsonl" {
        var handle: FileHandle
        if path.isEmpty || path == "-" {
            handle = FileHandle.standardOutput
        } else {
            FileManager.default.createFile(atPath: path, contents: nil)
            handle = FileHandle(forWritingAtPath: path)!
        }
        for row in rows {
            if let data = try? JSONSerialization.data(withJSONObject: row),
               let strData = String(data: data, encoding: .utf8)?.data(using: .utf8) {
                handle.write(strData)
                handle.write(Data([0x0a]))
            }
        }
        if handle !== FileHandle.standardOutput {
            handle.closeFile()
        }
    }
}
struct Auto1: Equatable {
    var age: Int
    var name: String
}

var people = [Auto1(age: 30, name: "Alice"), Auto1(age: 25, name: "Bob")]
_save(people, path: "-", opts: ["format": "jsonl"])
