// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:55:04Z
func intSqrt(_ x: Int) -> Int {
    if x < 2 {
        return x
    }
    var left = 1
    var right = x / 2
    var ans = 0
    while left <= right {
        let mid = left + (right - left) / 2
        let sq = mid * mid
        if sq == x {
            return mid
        }
        if sq < x {
            left = mid + 1
            ans = mid
        }
        else {
            right = mid - 1
        }
    }
    return ans
}
func sumRecip(_ n: Int) -> Int {
    var s = 1
    let limit = intSqrt(n)
    var f = 2
    while f <= limit {
        if n % f == 0 {
            s = s + n / f
            let f2 = n / f
            if f2 != f {
                s = s + f
            }
        }
        f = f + 1
    }
    return s
}
func main() {
    var nums = [6, 28, 120, 496, 672, 8128, 30240, 32760, 523776]
    for n in nums {
        let s = sumRecip(n)
        if s % n == 0 {
            let val = s / n
            var perfect = ""
            if val == 1 {
                perfect = "perfect!"
            }
            print("Sum of recipr. factors of " + String(n) + " = " + String(val) + " exactly " + perfect)
        }
    }
}
main()
