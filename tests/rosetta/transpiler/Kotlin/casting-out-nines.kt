import java.math.BigInteger

fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
}

var testCases: MutableList<MutableMap<String, Any?>> = mutableListOf(mutableMapOf<String, Any?>("base" to (10), "begin" to ("1"), "end" to ("100"), "kaprekar" to (mutableListOf("1", "9", "45", "55", "99"))), mutableMapOf<String, Any?>("base" to (17), "begin" to ("10"), "end" to ("gg"), "kaprekar" to (mutableListOf("3d", "d4", "gg"))))
var idx: Int = 0
fun parseIntBase(s: String, base: Int): Int {
    var digits: String = "0123456789abcdefghijklmnopqrstuvwxyz"
    var n: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var j: Int = 0
        var v: Int = 0
        while (j < digits.length) {
            if (digits.substring(j, j + 1) == s.substring(i, i + 1)) {
                v = j
                break
            }
            j = j + 1
        }
        n = (n * base) + v
        i = i + 1
    }
    return n
}

fun intToBase(n: Int, base: Int): String {
    var digits: String = "0123456789abcdefghijklmnopqrstuvwxyz"
    if (n == 0) {
        return "0"
    }
    var out: String = ""
    var v: Int = n
    while (v > 0) {
        var d: BigInteger = (Math.floorMod(v, base)).toBigInteger()
        out = digits.substring((d).toInt(), (d.add((1).toBigInteger())).toInt()) + out
        v = v / base
    }
    return out
}

fun subset(base: Int, begin: String, end: String): MutableList<String> {
    var b: Int = parseIntBase(begin, base)
    var e: Int = parseIntBase(end, base)
    var out: MutableList<String> = mutableListOf<String>()
    var k: Int = b
    while (k <= e) {
        var ks: String = intToBase(k, base)
        var mod: BigInteger = (base - 1).toBigInteger()
        var r1: BigInteger = (parseIntBase(ks, base)).toBigInteger().remainder((mod))
        var r2: BigInteger = ((parseIntBase(ks, base) * parseIntBase(ks, base))).toBigInteger().remainder((mod))
        if (r1.compareTo((r2)) == 0) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(ks); _tmp }
        }
        k = k + 1
    }
    return out
}

fun main() {
    while (idx < testCases.size) {
        var tc: MutableMap<String, Any?> = testCases[idx] as MutableMap<String, Any?>
        println(((((("\nTest case base = " + ((tc)["base"] as Any?).toString()) + ", begin = ") + ((tc)["begin"] as Any?).toString()) + ", end = ") + ((tc)["end"] as Any?).toString()) + ":")
        var s: MutableList<String> = subset((((tc)["base"] as Any?) as Int), (((tc)["begin"] as Any?) as String), (((tc)["end"] as Any?) as String))
        println("Subset:  " + s.toString())
        println("Kaprekar:" + ((tc)["kaprekar"] as Any?).toString())
        var sx: Int = 0
        var valid: Boolean = true
        var i: Int = 0
        while (i < _len((tc)["kaprekar"] as Any?)) {
            var k = ((((tc)["kaprekar"] as Any?) as Any?) as MutableList<Any?>)[i]
            var found: Boolean = false
            while (sx < s.size) {
                if (s[sx]!! == k) {
                    found = true
                    sx = sx + 1
                    break
                }
                sx = sx + 1
            }
            if (!found) {
                println(("Fail:" + (k).toString()) + " not in subset")
                valid = false
                break
            }
            i = i + 1
        }
        if ((valid as Boolean)) {
            println("Valid subset.")
        }
        idx = idx + 1
    }
}
