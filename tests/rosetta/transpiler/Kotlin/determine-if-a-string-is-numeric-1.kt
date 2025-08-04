fun isNumeric(s: String): Boolean {
    if (s == "NaN") {
        return true
    }
    var i: Int = 0
    if (s.length == 0) {
        return false
    }
    if ((s[0].toString() == "+") || (s[0].toString() == "-")) {
        if (s.length == 1) {
            return false
        }
        i = 1
    }
    var digits: Boolean = false
    var dot: Boolean = false
    while (i < s.length) {
        var ch: String = s[i].toString()
        if ((ch >= "0") && (ch <= "9")) {
            digits = true
            i = i + 1
        } else {
            if ((ch == ".") && (dot == false)) {
                dot = true
                i = i + 1
            } else {
                if ((((ch == "e") || (ch == "E") as Boolean)) && digits) {
                    i = i + 1
                    if ((i < s.length) && (((s[i].toString() == "+") || (s[i].toString() == "-") as Boolean))) {
                        i = i + 1
                    }
                    var ed: Boolean = false
                    while ((((i < s.length) && (s[i].toString() >= "0") as Boolean)) && (s[i].toString() <= "9")) {
                        ed = true
                        i = i + 1
                    }
                    return ((ed && (i == s.length)) as Boolean)
                } else {
                    return false
                }
            }
        }
    }
    return digits
}

fun user_main(): Unit {
    println("Are these strings numeric?")
    var strs: MutableList<String> = mutableListOf("1", "3.14", "-100", "1e2", "NaN", "rose")
    for (s in strs) {
        println((("  " + s) + " -> ") + isNumeric(s).toString())
    }
}

fun main() {
    user_main()
}
