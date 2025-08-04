fun isInt(s: String): Boolean {
    if (s.length == 0) {
        return false
    }
    for (_ch in s) {
        val ch = _ch.toString()
        if ((ch < "0") || (ch > "9")) {
            return false
        }
    }
    return true
}

fun user_main(): Unit {
    println("Are these strings integers?")
    var v: String = "1"
    var b: Boolean = false
    if (((isInt(v)) as Boolean)) {
        b = true
    }
    println((("  " + v) + " -> ") + b.toString())
    var i: String = "one"
    println((("  " + i) + " -> ") + isInt(i).toString())
}

fun main() {
    user_main()
}
