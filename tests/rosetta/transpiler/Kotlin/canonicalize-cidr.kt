fun repeat(s: String, n: Int): String {
    val sb = StringBuilder()
    repeat(n) { sb.append(s) }
    return sb.toString()
}

var tests: MutableList<String> = mutableListOf("87.70.141.1/22", "36.18.154.103/12", "62.62.197.11/29", "67.137.119.181/4", "161.214.74.21/24", "184.232.176.184/18")
fun split(s: String, sep: String): MutableList<String> {
    var parts: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        if ((((sep.length > 0) && ((i + sep.length) <= s.length) as Boolean)) && (s.substring(i, i + sep.length) == sep)) {
            parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
            cur = ""
            i = i + sep.length
        } else {
            cur = cur + s.substring(i, i + 1)
            i = i + 1
        }
    }
    parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
    return parts
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]!!
        i = i + 1
    }
    return res
}

fun repeat(ch: String, n: Int): String {
    var out: String = ""
    var i: Int = 0
    while (i < n) {
        out = out + ch
        i = i + 1
    }
    return out
}

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str.substring(i, i + 1)] as Int
        i = i + 1
    }
    if ((neg as Boolean)) {
        n = 0 - n
    }
    return n
}

fun toBinary(n: Int, bits: Int): String {
    var b: String = ""
    var _val: Int = n
    var i: Int = 0
    while (i < bits) {
        b = (Math.floorMod(_val, 2)).toString() + b
        _val = ((_val / 2).toInt())
        i = i + 1
    }
    return b
}

fun binToInt(bits: String): Int {
    var n: Int = 0
    var i: Int = 0
    while (i < bits.length) {
        n = (n * 2) + Integer.parseInt(bits.substring(i, i + 1), 10)
        i = i + 1
    }
    return n
}

fun padRight(s: String, width: Int): String {
    var out: String = s
    while (out.length < width) {
        out = out + " "
    }
    return out
}

fun canonicalize(cidr: String): String {
    var parts: MutableList<String> = split(cidr, "/")
    var dotted: String = parts[0]!!
    var size: (Int) -> Int = Integer.parseInt(parts[1]!!, 10)
    var binParts: MutableList<String> = mutableListOf<String>()
    for (p in split(dotted, ".")) {
        binParts = run { val _tmp = binParts.toMutableList(); _tmp.add(toBinary(((Integer.parseInt(p, 10)) as Int), 8)); _tmp }
    }
    var binary: String = join(binParts, "")
    binary = binary.substring(0, size) + repeat("0", 32 - size)
    var canonParts: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < binary.length) {
        canonParts = run { val _tmp = canonParts.toMutableList(); _tmp.add(binToInt(binary.substring(i, i + 8)).toString()); _tmp }
        i = i + 8
    }
    return (join(canonParts, ".") + "/") + parts[1]!!
}

fun main() {
    for (t in tests) {
        println((padRight(t, 18) + " -> ") + canonicalize(t))
    }
}
