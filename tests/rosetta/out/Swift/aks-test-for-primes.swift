// Generated by Mochi compiler v0.10.26 on 2025-07-16T13:16:02Z
func poly(_ p: Int) -> String {
    var s: String = ""
    var coef: Int = 1
    var i = p
    if coef != 1 {
        s = s + String(coef)
    }
    while i > 0 {
        s = s + "x"
        if i != 1 {
            s = s + "^" + String(i)
        }
        coef = Int((coef * i / (p - i + 1)))
        var d = coef
        if (p - (i - 1)) % 2 == 1 {
            d = -d
        }
        if d < 0 {
            s = s + " - " + String(-d)
        }
        else {
            s = s + " + " + String(d)
        }
        i = i - 1
    }
    if s == "" {
        s = "1"
    }
    return s
}
func aks(_ n: Int) -> Bool {
    if n < 2 {
        return false
    }
    var c: Int = n
    var i = 1
    while i < n {
        if c % n != 0 {
            return false
        }
        c = Int((c * (n - i) / (i + 1)))
        i = i + 1
    }
    return true
}
func main() {
    var p = 0
    while p <= 7 {
        print(String(p) + ":  " + poly(p))
        p = p + 1
    }
    var first = true
    p = 2
    var line: String = ""
    while p < 50 {
        if aks(p) {
            if first {
                line = line + String(p)
                first = false
            }
            else {
                line = line + " " + String(p)
            }
        }
        p = p + 1
    }
    print(line)
}
main()
