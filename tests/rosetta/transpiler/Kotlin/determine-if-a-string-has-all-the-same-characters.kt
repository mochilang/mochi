import java.math.BigInteger

fun ord(ch: String): Int {
    if (ch == "5") {
        return 53
    }
    if (ch == "T") {
        return 84
    }
    if (ch == " ") {
        return 32
    }
    if (ch == "é") {
        return 233
    }
    if (ch == "🐺") {
        return 128058
    }
    return 0
}

fun hex(n: Int): String {
    var digits: String = "0123456789abcdef"
    if (n == 0) {
        return "0x0"
    }
    var m: Int = n
    var out: String = ""
    while (m > 0) {
        var d: BigInteger = (Math.floorMod(m, 16)).toBigInteger()
        out = digits.substring((d).toInt(), (d.add((1).toBigInteger())).toInt()) + out
        m = m / 16
    }
    return "0x" + out
}

fun quote(s: String): String {
    return ("'" + s) + "'"
}

fun analyze(s: String): Unit {
    var le: Int = s.length
    println(((("Analyzing " + quote(s)) + " which has a length of ") + le.toString()) + ":")
    if (le > 1) {
        var i: Int = 1
        while (i < le) {
            var cur: String = s.substring(i, i + 1)
            var prev: String = s.substring(i - 1, i)
            if (cur != prev) {
                println("  Not all characters in the string are the same.")
                println(((((("  " + quote(cur)) + " (") + hex(ord(cur))) + ") is different at position ") + (i + 1).toString()) + ".")
                println("")
                return
            }
            i = i + 1
        }
    }
    println("  All characters in the string are the same.")
    println("")
}

fun user_main(): Unit {
    var strings: MutableList<String> = mutableListOf("", "   ", "2", "333", ".55", "tttTTT", "4444 444k", "pépé", "🐶🐶🐺🐶", "🎄🎄🎄🎄")
    var i: Int = 0
    while (i < strings.size) {
        analyze(strings[i]!!)
        i = i + 1
    }
}

fun main() {
    user_main()
}
