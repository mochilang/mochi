// Generated by Mochi transpiler v0.10.39 on 2025-07-25 09:13:41 GMT+7
import Foundation

func bigTrim(_ a: inout [Int]) -> [Int] {
    var n = Int(((a).count))
    while ((n > 1) && (Int(a[(n - 1)]) == 0)) {
        a = (Array(a[0..<(n - 1)]) as! [Int])
        n = ((n - 1) as! Int)
    }
    return (a as! [Int])
}
func bigFromInt(_ x: Int) -> [Int] {
    if (x == 0) {
        return ([0] as! [Int])
    }
    var digits: [Int] = ([] as! [Int])
    var n = (x as! Int)
    while (n > 0) {
        digits = ((digits + [(n % 10)]) as! [Int])
        n = ((n / 10) as! Int)
    }
    return (digits as! [Int])
}
func bigAdd(_ a: [Int], _ b: [Int]) -> [Int] {
    var res: [Int] = ([] as! [Int])
    var carry = 0
    var i = 0
    while (((i < Int(((a).count))) || (i < Int(((b).count)))) || (carry > 0)) {
        var av = 0
        if (i < Int(((a).count))) {
            av = Int(a[i])
        }
        var bv = 0
        if (i < Int(((b).count))) {
            bv = Int(b[i])
        }
        var s = (((av + bv) + carry) as! Int)
        res = ((res + [(s % 10)]) as! [Int])
        carry = ((s / 10) as! Int)
        i = ((i + 1) as! Int)
    }
    return (bigTrim(&res) as! [Int])
}
func bigSub(_ a: [Int], _ b: [Int]) -> [Int] {
    var res: [Int] = ([] as! [Int])
    var borrow = 0
    var i = 0
    while (i < Int(((a).count))) {
        var av = Int(a[i])
        var bv = 0
        if (i < Int(((b).count))) {
            bv = Int(b[i])
        }
        var diff = (((av - bv) - borrow) as! Int)
        if (diff < 0) {
            diff = ((diff + 10) as! Int)
            borrow = 1
        } else {
            borrow = 0
        }
        res = ((res + [diff]) as! [Int])
        i = ((i + 1) as! Int)
    }
    return (bigTrim(&res) as! [Int])
}
func bigToString(_ a: [Int]) -> String {
    var s = ""
    var i = ((Int(((a).count)) - 1) as! Int)
    while (i >= 0) {
        s = ((s + String(describing: Int(a[i]))) as! String)
        i = ((i - 1) as! Int)
    }
    return (s as! String)
}
func minInt(_ a: Int, _ b: Int) -> Int {
    if (a < b) {
        return (a as! Int)
    } else {
        return (b as! Int)
    }
}
func cumu(_ n: Int) -> [[Int]] {
    var cache: [[[Int]]] = ([([(bigFromInt(1) as! [Int])] as! [[Int]])] as! [[[Int]]])
    var y = 1
    while (y <= n) {
        var row: [[Int]] = ([(bigFromInt(0) as! [Int])] as! [[Int]])
        var x = 1
        while (x <= y) {
            let val = (cache[(y - x)][Int(minInt((x as! Int), ((y - x) as! Int)))] as! [Int])
            row = ((row + [(bigAdd((row[(Int(((row).count)) - 1)] as! [Int]), (val as! [Int])) as! [Int])]) as! [[Int]])
            x = ((x + 1) as! Int)
        }
        cache = ((cache + [row]) as! [[[Int]]])
        y = ((y + 1) as! Int)
    }
    return (cache[n] as! [[Int]])
}
func row(_ n: Int) -> [String] {
    let e = (cumu((n as! Int)) as! [[Int]])
    var out: [String] = ([] as! [String])
    var i = 0
    while (i < n) {
        let diff = (bigSub((e[(i + 1)] as! [Int]), (e[i] as! [Int])) as! [Int])
        out = ((out + [String(describing: bigToString((diff as! [Int])))]) as! [String])
        i = ((i + 1) as! Int)
    }
    return (out as! [String])
}
print("rows:")
var x = 1
while (x < 11) {
    let r = (row((x as! Int)) as! [String])
    var line = ""
    var i = 0
    while (i < Int(((r).count))) {
        line = ((((line + " ") + String(describing: r[i])) + " ") as! String)
        i = ((i + 1) as! Int)
    }
    print(line)
    x = ((x + 1) as! Int)
}
print("")
print("sums:")
for num in ([23, 123, 1234] as! [Int]) {
    let r = (cumu((num as! Int)) as! [[Int]])
    print(((String(describing: num) + " ") + String(describing: bigToString((r[(Int(((r).count)) - 1)] as! [Int])))))
}
