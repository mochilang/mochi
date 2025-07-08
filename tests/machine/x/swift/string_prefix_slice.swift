let prefix = "fore"
let s1 = "forest"
print(String(s1[s1.index(s1.startIndex, offsetBy: 0)..<s1.index(s1.startIndex, offsetBy: prefix.count)]) == prefix)
let s2 = "desert"
print(String(s2[s2.index(s2.startIndex, offsetBy: 0)..<s2.index(s2.startIndex, offsetBy: prefix.count)]) == prefix)
