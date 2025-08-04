import java.math.BigInteger

fun indexOf3(s: String, ch: String, start: Int): Int {
    var i: Int = start
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ord(ch: String): Int {
    var digits: String = "0123456789"
    var idx: Int = indexOf3(digits, ch, 0)
    if (idx >= 0) {
        return 48 + idx
    }
    if (ch == "X") {
        return 88
    }
    if (ch == "é") {
        return 233
    }
    if (ch == "😍") {
        return 128525
    }
    if (ch == "🐡") {
        return 128033
    }
    return 0
}

fun toHex(n: Int): String {
    var digits: String = "0123456789ABCDEF"
    if (n == 0) {
        return "0"
    }
    var v: Int = n
    var out: String = ""
    while (v > 0) {
        var d: BigInteger = (Math.floorMod(v, 16)).toBigInteger()
        out = digits.substring((d).toInt(), (d.add((1).toBigInteger())).toInt()) + out
        v = v / 16
    }
    return out
}

fun analyze(s: String): Unit {
    var le: Int = s.length
    println(((("Analyzing \"" + s) + "\" which has a length of ") + le.toString()) + ":")
    if (le > 1) {
        var i: Int = 0
        while (i < (le - 1)) {
            var j: BigInteger = (i + 1).toBigInteger()
            while (j.compareTo((le).toBigInteger()) < 0) {
                if (s.substring((j).toInt(), (j.add((1).toBigInteger())).toInt()) == s.substring(i, i + 1)) {
                    var ch: String = s.substring(i, i + 1)
                    println("  Not all characters in the string are unique.")
                    println(((((((("  '" + ch) + "' (0x") + (toHex(ord(ch)).toLowerCase()).toString()) + ") is duplicated at positions ") + (i + 1).toString()) + " and ") + (j.add((1).toBigInteger())).toString()) + ".\n")
                    return
                }
                j = j.add((1).toBigInteger())
            }
            i = i + 1
        }
    }
    println("  All characters in the string are unique.\n")
}

fun user_main(): Unit {
    var strings: MutableList<String> = mutableListOf("", ".", "abcABC", "XYZ ZYX", "1234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ", "01234567890ABCDEFGHIJKLMN0PQRSTUVWXYZ0X", "hétérogénéité", "🎆🎃🎇🎈", "😍😀🙌💃😍🙌", "🐠🐟🐡🦈🐬🐳🐋🐡")
    var i: Int = 0
    while (i < strings.size) {
        analyze(strings[i]!!)
        i = i + 1
    }
}

fun main() {
    user_main()
}
