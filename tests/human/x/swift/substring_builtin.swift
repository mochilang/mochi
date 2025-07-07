let str = "mochi"
let start = str.index(str.startIndex, offsetBy: 1)
let end = str.index(str.startIndex, offsetBy: 4)
print(String(str[start..<end]))
