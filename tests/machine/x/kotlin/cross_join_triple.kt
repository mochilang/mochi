
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}

fun avg(list: List<Any?>): Double {
    if (list.isEmpty()) return 0.0
    var s = 0.0
    for (n in list) s += toDouble(n)
    return s / list.size
}

fun count(list: Collection<Any?>): Int = list.size

fun exists(list: Collection<Any?>): Boolean = list.isNotEmpty()

fun <T> values(m: Map<*, T>): MutableList<T> = m.values.toMutableList()

fun len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> 0
}

fun max(list: List<Any?>): Int {
    var m = Int.MIN_VALUE
    for (n in list) {
        val v = toInt(n)
        if (v > m) m = v
    }
    return if (m == Int.MIN_VALUE) 0 else m
}

fun min(list: List<Any?>): Int {
    var m = Int.MAX_VALUE
    for (n in list) {
        val v = toInt(n)
        if (v < m) m = v
    }
    return if (m == Int.MAX_VALUE) 0 else m
}

fun sum(list: List<Any?>): Int {
    var s = 0
    for (n in list) s += toInt(n)
    return s
}

fun str(v: Any?): String = v.toString()

fun substring(s: String, start: Int, end: Int): String = s.substring(start, end)

fun toInt(v: Any?): Int = when (v) {
    is Int -> v
    is Double -> v.toInt()
    is String -> v.toInt()
    is Boolean -> if (v) 1 else 0
    else -> 0
}

fun toDouble(v: Any?): Double = when (v) {
    is Double -> v
    is Int -> v.toDouble()
    is String -> v.toDouble()
    else -> 0.0
}

fun toBool(v: Any?): Boolean = when (v) {
    is Boolean -> v
    is Int -> v != 0
    is Double -> v != 0.0
    is String -> v.isNotEmpty()
    null -> false
    else -> true
}

fun <T> union(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = a.toMutableList()
    for (x in b) if (!res.contains(x)) res.add(x)
    return res
}

fun <T> except(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (!b.contains(x)) res.add(x)
    return res
}

fun <T> intersect(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    for (x in a) if (b.contains(x)) res.add(x)
    return res
}

fun _load(path: String?, opts: Map<String, Any?>?): MutableList<MutableMap<String, Any?>> {
    val fmt = opts?.get("format") as? String ?: "csv"
    val lines = if (path == null || path == "-") {
        listOf<String>()
    } else {
        java.io.File(path).readLines()
    }
    return when (fmt) {
        "yaml" -> loadYamlSimple(lines)
        else -> mutableListOf()
    }
}

fun loadYamlSimple(lines: List<String>): MutableList<MutableMap<String, Any?>> {
    val res = mutableListOf<MutableMap<String, Any?>>()
    var cur: MutableMap<String, Any?>? = null
    for (ln in lines) {
        val t = ln.trim()
        if (t.startsWith("- ")) {
            cur?.let { res.add(it) }
            cur = mutableMapOf()
            val idx = t.indexOf(':', 2)
            if (idx >= 0) {
                val k = t.substring(2, idx).trim()
                val v = parseSimpleValue(t.substring(idx + 1))
                cur!![k] = v
            }
        } else if (t.contains(':')) {
            val idx = t.indexOf(':')
            val k = t.substring(0, idx).trim()
            val v = parseSimpleValue(t.substring(idx + 1))
            cur?.set(k, v)
        }
    }
    cur?.let { res.add(it) }
    return res
}

fun parseSimpleValue(s: String): Any? {
    val t = s.trim()
    return when {
        t.matches(Regex("^-?\\d+$")) -> t.toInt()
        t.matches(Regex("^-?\\d+\\.\\d+$")) -> t.toDouble()
        t.equals("true", true) -> true
        t.equals("false", true) -> false
        t.startsWith("\"") && t.endsWith("\"") -> t.substring(1, t.length - 1)
        else -> t
    }
}

fun _save(rows: List<Any?>, path: String?, opts: Map<String, Any?>?) {
    val fmt = opts?.get("format") as? String ?: "csv"
    val writer = if (path == null || path == "-") {
        java.io.BufferedWriter(java.io.OutputStreamWriter(System.out))
    } else {
        java.io.File(path).bufferedWriter()
    }
    if (fmt == "jsonl") {
        for (r in rows) {
            writer.write(toJson(r))
            writer.newLine()
        }
    }
    if (path != null && path != "-") writer.close()
}

fun json(v: Any?) {
    println(toJson(v))
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

class Group(val key: Any?, val items: MutableList<Any?>) : MutableList<Any?> by items


val nums = mutableListOf(1, 2)

val letters = mutableListOf("A", "B")

val bools = mutableListOf(true, false)

val combos = run {
    val __res = mutableListOf<MutableMap<Any?, Any?>>()
    for (n in nums) {
        for (l in letters) {
            for (b in bools) {
                __res.add((mutableMapOf("n" to n, "l" to l, "b" to b) as MutableMap<Any?, Any?>))
            }
        }
    }
    __res
}

fun main() {
    println("--- Cross Join of three lists ---")
    for (c in combos) {
        println(listOf((c as MutableMap<*, *>)["n"], (c as MutableMap<*, *>)["l"], (c as MutableMap<*, *>)["b"]).joinToString(" "))
    }
}
