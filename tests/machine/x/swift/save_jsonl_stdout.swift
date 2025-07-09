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
let people = [["name": "Alice", "age": 30], ["name": "Bob", "age": 25]]
_save(people, path: "-", opts: ["format": "jsonl"])
