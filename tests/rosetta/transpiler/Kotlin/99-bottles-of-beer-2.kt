fun fields(s: String): MutableList<String> {
    var words: MutableList<String> = mutableListOf()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        val ch: String = s.substring(i, i + 1)
        if ((((ch == " ") || (ch == "\n") as Boolean)) || (ch == "\t")) {
            if (cur.length > 0) {
                words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
                cur = ""
            }
        } else {
            cur = cur + ch
        }
        i = i + 1
    }
    if (cur.length > 0) {
        words = run { val _tmp = words.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    }
    return words
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]
        i = i + 1
    }
    return res
}

fun numberName(n: Int): String {
    val small: MutableList<String> = mutableListOf("no", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
    val tens: MutableList<String> = mutableListOf("ones", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    if (n < 0) {
        return ""
    }
    if (n < 20) {
        return small[n]
    }
    if (n < 100) {
        var t: String = tens[(n / 10).toInt()]
        var s: Int = n % 10
        if (s > 0) {
            t = (t + " ") + small[s]
        }
        return t
    }
    return ""
}

fun pluralizeFirst(s: String, n: Int): String {
    if (n == 1) {
        return s
    }
    val w: MutableList<String> = fields(s)
    if (w.size > 0) {
        w[0] = w[0] + "s"
    }
    return join(w, " ")
}

fun randInt(seed: Int, n: Int): Int {
    val next: Int = ((seed * 1664525) + 1013904223) % 2147483647
    return next % n
}

fun slur(p: String, d: Int): String {
    if (p.length <= 2) {
        return p
    }
    var a: MutableList<String> = mutableListOf()
    var i: Int = 1
    while (i < (p.length - 1)) {
        a = run { val _tmp = a.toMutableList(); _tmp.add(p.substring(i, i + 1)); _tmp } as MutableList<String>
        i = i + 1
    }
    var idx: Int = a.size - 1
    var seed: Int = d
    while (idx >= 1) {
        seed = ((seed * 1664525) + 1013904223) % 2147483647
        if ((seed % 100) >= d) {
            val j: Int = seed % (idx + 1)
            val tmp: String = a[idx]
            a[idx] = a[j]
            a[j] = tmp
        }
        idx = idx - 1
    }
    var s: String = p.substring(0, 1)
    var k: Int = 0
    while (k < a.size) {
        s = s + a[k]
        k = k + 1
    }
    s = s + (p.substring(p.length - 1, p.length)).toString()
    val w: MutableList<String> = fields(s)
    return join(w, " ")
}

fun user_main(): Unit {
    var i: Int = 99
    while (i > 0) {
        println((((slur(numberName(i), i) + " ") + pluralizeFirst(slur("bottle of", i), i)) + " ") + slur("beer on the wall", i))
        println((((slur(numberName(i), i) + " ") + pluralizeFirst(slur("bottle of", i), i)) + " ") + slur("beer", i))
        println((((slur("take one", i) + " ") + slur("down", i)) + " ") + slur("pass it around", i))
        println((((slur(numberName(i - 1), i) + " ") + pluralizeFirst(slur("bottle of", i), i - 1)) + " ") + slur("beer on the wall", i))
        i = i - 1
    }
}

fun main() {
    user_main()
}
