import Foundation

public func mochiHttpGet(_ urlString: String) -> String {
    guard let url = URL(string: urlString) else { return "" }
    guard let data = try? Data(contentsOf: url) else { return "" }
    return (String(data: data, encoding: .utf8) ?? "").trimmingCharacters(in: .newlines)
}
