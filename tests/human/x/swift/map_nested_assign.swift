var data: [String: [String: Int]] = ["outer": ["inner": 1]]
if var inner = data["outer"] {
    inner["inner"] = 2
    data["outer"] = inner
}
print(data["outer"]!["inner"]!)
