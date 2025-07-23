fun poly(p: Int): String {
    var s: String = ""
    var coef: Int = 1
    var i: Int = p
    if (coef != 1) {
        s = s + coef.toString()
    }
    while (i > 0) {
        s = s + "x"
        if (i != 1) {
            s = (s + "^") + i.toString()
        }
        coef = (coef * i) / ((p - i) + 1).toInt() as Int
        var d: Int = coef
        if (((p - (i - 1)) % 2) == 1) {
            d = 0 - d
        }
        if (d < 0) {
            s = (s + " - ") + (0 - d).toString()
        } else {
            s = (s + " + ") + d.toString()
        }
        i = i - 1
    }
    if (s == "") {
        s = "1"
    }
    return s
}

fun aks(n: Int): Boolean {
    if (n < 2) {
        return false
    }
    var c: Int = n
    var i: Int = 1
    while (i < n) {
        if ((c % n) != 0) {
            return false
        }
        c = (c * (n - i)) / (i + 1).toInt() as Int
        i = i + 1
    }
    return true
}

fun user_main(): Unit {
    var p: Int = 0
    while (p <= 7) {
        println((p.toString() + ":  ") + poly(p) as String)
        p = p + 1
    }
    var first: Boolean = true
    p = 2
    var line: String = ""
    while (p < 50) {
        if (aks(p) as Boolean as Boolean) {
            if (first as Boolean) {
                line = line + p.toString()
                first = false
            } else {
                line = (line + " ") + p.toString()
            }
        }
        p = p + 1
    }
    println(line)
}

fun main() {
    user_main()
}
