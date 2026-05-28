import Foundation

public func mochiLLMGenerate(_ provider: String, _ model: String, _ prompt: String) -> String {
    if let cassetteDir = ProcessInfo.processInfo.environment["MOCHI_LLM_CASSETTE_DIR"] {
        let key = mochiDJB2Key(provider, model, prompt)
        let path = "\(cassetteDir)/\(key).txt"
        if let content = try? String(contentsOfFile: path, encoding: .utf8) {
            return content.trimmingCharacters(in: .newlines)
        }
        fputs("mochi_llm: cassette not found: \(path)\n", stderr)
        return ""
    }
    fputs("mochi_llm: MOCHI_LLM_CASSETTE_DIR not set; set it to a cassette directory or provide an API key\n", stderr)
    return ""
}

private func mochiDJB2Key(_ provider: String, _ model: String, _ prompt: String) -> UInt64 {
    var h: UInt64 = 5381
    for b in (provider + "\0" + model + "\0" + prompt).utf8 {
        h = (h &* 33) ^ UInt64(b)
    }
    return h
}
