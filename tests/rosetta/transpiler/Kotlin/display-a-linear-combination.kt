fun padRight(s: String, w: Int): String {
    var r: String = s
    while (r.length < w) {
        r = r + " "
    }
    return r
}

fun linearCombo(c: MutableList<Int>): String {
    var out: String = ""
    var i: Int = 0
    while (i < c.size) {
        var n: Int = c[i]!!
        if (n != 0) {
            var op: String = ""
            if ((n < 0) && (out.length == 0)) {
                op = "-"
            } else {
                if (n < 0) {
                    op = " - "
                } else {
                    if ((n > 0) && (out.length == 0)) {
                        op = ""
                    } else {
                        op = " + "
                    }
                }
            }
            var av: Int = n
            if (av < 0) {
                av = 0 - av
            }
            var coeff: String = av.toString() + "*"
            if (av == 1) {
                coeff = ""
            }
            out = ((((out + op) + coeff) + "e(") + (i + 1).toString()) + ")"
        }
        i = i + 1
    }
    if (out.length == 0) {
        return "0"
    }
    return out
}

fun user_main(): Unit {
    var combos: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(1, 2, 3), mutableListOf(0, 1, 2, 3), mutableListOf(1, 0, 3, 4), mutableListOf(1, 2, 0), mutableListOf(0, 0, 0), mutableListOf(0), mutableListOf(1, 1, 1), mutableListOf(0 - 1, 0 - 1, 0 - 1), mutableListOf(0 - 1, 0 - 2, 0, 0 - 3), mutableListOf(0 - 1))
    var idx: Int = 0
    while (idx < combos.size) {
        var c: MutableList<Int> = combos[idx]!!
        var t: String = "["
        var j: Int = 0
        while (j < c.size) {
            t = t + (c[j]!!).toString()
            if (j < (c.size - 1)) {
                t = t + ", "
            }
            j = j + 1
        }
        t = t + "]"
        var lc: String = linearCombo(c)
        println((padRight(t, 15) + "  ->  ") + lc)
        idx = idx + 1
    }
}

fun main() {
    user_main()
}
