let prefix = "fore"
let s1 = "forest"
let start = s1.startIndex
let end = s1.index(start, offsetBy: prefix.count)
print(String(s1[start..<end]) == prefix)
let s2 = "desert"
print(String(s2[start..<s2.index(start, offsetBy: prefix.count)]) == prefix)
