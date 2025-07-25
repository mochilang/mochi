// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}
// Code generated from burrows-wheeler-transform.mochi

val stx = "\x02"

val etx = "\x03"

/**
 * Auto-generated from Mochi
 * @param s String
 * @param ch String
 * @return Boolean
 */
fun contains(s: String, ch: String): Boolean {
    var i = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return true
        }
        i = i + 1
    }
    return false
}

/**
 * Auto-generated from Mochi
 * @param xs MutableList<String>
 * @return MutableList<String>
 */
fun sortStrings(xs: MutableList<String>): MutableList<String> {
    var arr = xs
    var n = arr.size
    var i = 0
    while (i < n) {
        var j = 0
        while (j < n - 1) {
            if (arr[j] > arr[j + 1]) {
                val tmp = arr[j]
                arr[j] = arr[j + 1]
                arr[j + 1] = tmp
            }
            j = j + 1
        }
        i = i + 1
    }
    return arr
}

/**
 * Auto-generated from Mochi
 * @param s String
 * @return MutableMap<String, Any>
 */
fun bwt(s: String): MutableMap<String, Any> {
    if (contains(s, stx) || contains(s, etx)) {
        return mutableMapOf("err" to true, "res" to "")
    }
    s = stx + s + etx
    val le = s.length
    var table: MutableList<String> = mutableListOf<String>()
    var i = 0
    while (i < le) {
        val rot = s.substring(i, le) + s.substring(0, i)
        table = append(table, rot)
        i = i + 1
    }
    table = sortStrings(table)
    var last = ""
    i = 0
    while (i < le) {
        last = last + table[i].substring(le - 1, le)
        i = i + 1
    }
    return mutableMapOf("err" to false, "res" to last)
}

/**
 * Auto-generated from Mochi
 * @param r String
 * @return String
 */
fun ibwt(r: String): String {
    val le = r.length
    var table: MutableList<String> = mutableListOf<String>()
    var i = 0
    while (i < le) {
        table = append(table, "")
        i = i + 1
    }
    var n = 0
    while (n < le) {
        i = 0
        while (i < le) {
            table[i] = r.substring(i, i + 1) + table[i]
            i = i + 1
        }
        table = sortStrings(table)
        n = n + 1
    }
    i = 0
    while (i < le) {
        if (table[i].substring(le - 1, le) == etx) {
            return table[i].substring(1, le - 1)
        }
        i = i + 1
    }
    return ""
}

/**
 * Auto-generated from Mochi
 * @param s String
 * @return String
 */
fun makePrintable(s: String): String {
    var out = ""
    var i = 0
    while (i < s.length) {
        val ch = s.substring(i, i + 1)
        if (ch == stx) {
            out = out + "^"
        }
        else
        if (ch == etx) {
            out = out + "|"
        }
        else {
            out = out + ch
        }
        i = i + 1
    }
    return out
}

/**
 * Auto-generated from Mochi
 */
fun main(): Unit {
    val examples = mutableListOf("banana", "appellee", "dogwood", "TO BE OR NOT TO BE OR WANT TO BE OR NOT?", "SIX.MIXED.PIXIES.SIFT.SIXTY.PIXIE.DUST.BOXES", "\x02ABC\x03")
    for (t in examples) {
        println(makePrintable(t))
        val res = bwt(t)
        if (toBool(res["err"])) {
            println(" --> ERROR: String can't contain STX or ETX")
            println(" -->")
        }
        else {
            val enc = (res["res"] as Any?).toString()
            println(" --> " + makePrintable(enc))
            val r = ibwt(enc)
            println(" --> " + r)
        }
        println("")
    }
}

