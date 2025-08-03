fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun rotate(s: String, n: Int): String {
    return s.substring(n, s.length) + s.substring(0, n)
}

fun scrambleLeft(s: String): String {
    return ((s.substring(0, 1) + s.substring(2, 14)) + s.substring(1, 2)) + s.substring(14, s.length)
}

fun scrambleRight(s: String): String {
    return (((s.substring(1, 3) + s.substring(4, 15)) + s.substring(3, 4)) + s.substring(15, s.length)) + s.substring(0, 1)
}

fun chao(text: String, encode: Boolean): String {
    var left: String = "HXUCZVAMDSLKPEFJRIGTWOBNYQ"
    var right: String = "PTLNBQDEOYSFAVZKGJRIHWXUMC"
    var out: String = ""
    var i: Int = 0
    while (i < text.length) {
        var ch: String = text.substring(i, i + 1)
        var idx: Int = 0
        if ((encode as Boolean)) {
            idx = right.indexOf(ch)
            out = out + left.substring(idx, idx + 1)
        } else {
            idx = left.indexOf(ch)
            out = out + right.substring(idx, idx + 1)
        }
        left = rotate(left, idx)
        right = rotate(right, idx)
        left = scrambleLeft(left)
        right = scrambleRight(right)
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var plain: String = "WELLDONEISBETTERTHANWELLSAID"
    var cipher: String = chao(plain, true)
    println(plain)
    println(cipher)
    println(chao(cipher, false))
}

fun main() {
    user_main()
}
