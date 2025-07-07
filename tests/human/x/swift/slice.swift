let arr = [1,2,3]
print(Array(arr[1..<3]))
print(Array(arr[0..<2]))
let str = "hello"
let start = str.index(str.startIndex, offsetBy: 1)
let end = str.index(str.startIndex, offsetBy: 4)
print(String(str[start..<end]))
