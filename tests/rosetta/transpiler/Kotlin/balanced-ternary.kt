import java.math.BigInteger

fun trimLeftZeros(s: String): String {
    var i: Int = 0
    while ((i < s.length) && (s.substring(i, i + 1) == "0")) {
        i = i + 1
    }
    return s.substring(i, s.length) as String
}

fun btString(s: String): MutableMap<String, Any?> {
    var s: String = s
    s = trimLeftZeros(s)
    var b: MutableList<Int> = mutableListOf<Int>()
    var i: BigInteger = (s.length - 1).toBigInteger()
    while (i.compareTo(0.toBigInteger()) >= 0) {
        val ch: String = s.substring((i).toInt(), (i.add(1.toBigInteger())).toInt())
        if (ch == "+") {
            b = run { val _tmp = b.toMutableList(); _tmp.add(1); _tmp } as MutableList<Int>
        } else {
            if (ch == "0") {
                b = run { val _tmp = b.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            } else {
                if (ch == "-") {
                    b = run { val _tmp = b.toMutableList(); _tmp.add(0 - 1); _tmp } as MutableList<Int>
                } else {
                    return mutableMapOf<String, Any?>("bt" to (mutableListOf<Any?>()), "ok" to (false))
                }
            }
        }
        i = i.subtract(1.toBigInteger())
    }
    return mutableMapOf<String, Any?>("bt" to (b), "ok" to (true))
}

fun btToString(b: MutableList<Int>): String {
    if (b.size == 0) {
        return "0"
    }
    var r: String = ""
    var i: BigInteger = (b.size - 1).toBigInteger()
    while (i.compareTo(0.toBigInteger()) >= 0) {
        val d: Int = b[(i).toInt()]
        if (d == (0 - 1)) {
            r = r + "-"
        } else {
            if (d == 0) {
                r = r + "0"
            } else {
                r = r + "+"
            }
        }
        i = i.subtract(1.toBigInteger())
    }
    return r
}

fun btInt(i: Int): MutableList<Int> {
    if (i == 0) {
        return mutableListOf<Int>()
    }
    var n: Int = i
    var b: MutableList<Int> = mutableListOf<Int>()
    while (n != 0) {
        var m: BigInteger = (Math.floorMod(n, 3)).toBigInteger()
        n = (n / 3).toInt()
        if (m.compareTo(2.toBigInteger()) == 0) {
            m = (0 - 1).toBigInteger()
            n = n + 1
        } else {
            if (m.compareTo((0 - 2).toBigInteger()) == 0) {
                m = 1.toBigInteger()
                n = n - 1
            }
        }
        b = run { val _tmp = b.toMutableList(); _tmp.add(m.toInt()); _tmp } as MutableList<Int>
    }
    return b
}

fun btToInt(b: MutableList<Int>): Int {
    var r: Int = 0
    var pt: Int = 1
    var i: Int = 0
    while (i < b.size) {
        r = r + (b[i] * pt)
        pt = pt * 3
        i = i + 1
    }
    return r
}

fun btNeg(b: MutableList<Int>): MutableList<Int> {
    var r: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < b.size) {
        r = run { val _tmp = r.toMutableList(); _tmp.add(0 - b[i]); _tmp } as MutableList<Int>
        i = i + 1
    }
    return r
}

fun btAdd(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    return btInt(btToInt(a) + btToInt(b))
}

fun btMul(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    return btInt(btToInt(a) * btToInt(b))
}

fun padLeft(s: String, w: Int): String {
    var r: String = s
    while (r.length < w) {
        r = " " + r
    }
    return r
}

fun show(label: String, b: MutableList<Int>): Unit {
    val l: String = padLeft(label, 7)
    val bs: String = padLeft(btToString(b), 12)
    val _is: String = padLeft(btToInt(b).toString(), 7)
    println((((l + " ") + bs) + " ") + _is)
}

fun user_main(): Unit {
    val ares: MutableMap<String, Any?> = btString("+-0++0+")
    val a: Any? = (ares)["bt"] as Any?
    val b: MutableList<Int> = btInt(0 - 436)
    val cres: MutableMap<String, Any?> = btString("+-++-")
    val c: Any? = (cres)["bt"] as Any?
    show("a:", a as MutableList<Int>)
    show("b:", b)
    show("c:", c as MutableList<Int>)
    show("a(b-c):", btMul(a as MutableList<Int>, btAdd(b, btNeg(c as MutableList<Int>))))
}

fun main() {
    user_main()
}
