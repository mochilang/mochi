// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:55:23Z
func char(_ n: Int) -> String {
    let letters = "abcdefghijklmnopqrstuvwxyz"
    let idx = n - 97
    if idx < 0 || idx >= letters.count {
        return "?"
    }
    return String(letters[letters.index(letters.startIndex, offsetBy: idx)..<letters.index(letters.startIndex, offsetBy: idx + 1)])
}
func fromBytes(_ bs: [Int]) -> String {
    var s = ""
    var i = 0
    while i < bs.count {
        s = s + char(bs[i])
        i = i + 1
    }
    return s
}
var b: [Int] = [98, 105, 110, 97, 114, 121]
print(String(b))
var c: [Int] = b
print(String(c))
print(String(b == c))
var d: [Int] = [Any]()
var i = 0
while i < b.count {
    d = d + [b[i]]
    i = i + 1
}
d[1] = 97
d[4] = 110
print(fromBytes(b))
print(fromBytes(d))
print(String(b.count == 0))
var z = b + [122]
print(fromBytes(z))
var sub = Array(b[1..<3])
print(fromBytes(sub))
var f: [Int] = [Any]()
i = 0
while i < d.count {
    let val = d[i]
    if val == 110 {
        f = f + [109]
    }
    else {
        f = f + [val]
    }
    i = i + 1
}
print(fromBytes(d) + " -> " + fromBytes(f))
var rem: [Int] = [Any]()
rem = rem + [b[0]]
i = 3
while i < b.count {
    rem = rem + [b[i]]
    i = i + 1
}
print(fromBytes(rem))
