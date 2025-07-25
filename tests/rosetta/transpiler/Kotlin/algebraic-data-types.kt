var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

var tr: Any? = null
var i: Int = 1
fun node(cl: String, le: Any?, aa: Int, ri: Any?): MutableMap<String, Any?> {
    return mutableMapOf<String, Any?>("cl" to (cl), "le" to (le), "aa" to (aa), "ri" to (ri))
}

fun treeString(t: Any?): String {
    if (t == null) {
        return "E"
    }
    val m: MutableMap<String, Any?> = t as MutableMap<String, Any?>
    return ((((((("T(" + ((m)["cl"] as Any?).toString()) + ", ") + treeString((m)["le"] as Any?)) + ", ") + ((m)["aa"] as Any?).toString()) + ", ") + treeString((m)["ri"] as Any?)) + ")"
}

fun balance(t: Any?): Any? {
    if (t == null) {
        return t
    }
    val m: MutableMap<String, Any?> = t as MutableMap<String, Any?>
    if ((m)["cl"] as Any? != "B") {
        return t
    }
    val le: Any? = (m)["le"] as Any?
    val ri: Any? = (m)["ri"] as Any?
    if (le != null) {
        val leMap: MutableMap<String, Any?> = le as MutableMap<String, Any?>
        if ((leMap)["cl"] as Any? == "R") {
            val lele: Any? = (leMap)["le"] as Any?
            if (lele != null) {
                val leleMap: MutableMap<String, Any?> = lele as MutableMap<String, Any?>
                if ((leleMap)["cl"] as Any? == "R") {
                    return (node("R", (node("B", (leleMap)["le"] as Any?, ((leleMap)["aa"] as Any?) as Int, (leleMap)["ri"] as Any?)) as Any?, ((leMap)["aa"] as Any?) as Int, (node("B", (leMap)["ri"] as Any?, ((m)["aa"] as Any?) as Int, ri)) as Any?)) as Any?
                }
            }
            val leri: Any? = (leMap)["ri"] as Any?
            if (leri != null) {
                val leriMap: MutableMap<String, Any?> = leri as MutableMap<String, Any?>
                if ((leriMap)["cl"] as Any? == "R") {
                    return (node("R", (node("B", (leMap)["le"] as Any?, ((leMap)["aa"] as Any?) as Int, (leriMap)["le"] as Any?)) as Any?, ((leriMap)["aa"] as Any?) as Int, (node("B", (leriMap)["ri"] as Any?, ((m)["aa"] as Any?) as Int, ri)) as Any?)) as Any?
                }
            }
        }
    }
    if (ri != null) {
        val riMap: MutableMap<String, Any?> = ri as MutableMap<String, Any?>
        if ((riMap)["cl"] as Any? == "R") {
            val rile: Any? = (riMap)["le"] as Any?
            if (rile != null) {
                val rileMap: MutableMap<String, Any?> = rile as MutableMap<String, Any?>
                if ((rileMap)["cl"] as Any? == "R") {
                    return (node("R", (node("B", (m)["le"] as Any?, ((m)["aa"] as Any?) as Int, (rileMap)["le"] as Any?)) as Any?, ((rileMap)["aa"] as Any?) as Int, (node("B", (rileMap)["ri"] as Any?, ((riMap)["aa"] as Any?) as Int, (riMap)["ri"] as Any?)) as Any?)) as Any?
                }
            }
            val riri: Any? = (riMap)["ri"] as Any?
            if (riri != null) {
                val ririMap: MutableMap<String, Any?> = riri as MutableMap<String, Any?>
                if ((ririMap)["cl"] as Any? == "R") {
                    return (node("R", (node("B", (m)["le"] as Any?, ((m)["aa"] as Any?) as Int, (riMap)["le"] as Any?)) as Any?, ((riMap)["aa"] as Any?) as Int, (node("B", (ririMap)["le"] as Any?, ((ririMap)["aa"] as Any?) as Int, (ririMap)["ri"] as Any?)) as Any?)) as Any?
                }
            }
        }
    }
    return t
}

fun ins(tr: Any?, x: Int): Any? {
    if (tr == null) {
        return (node("R", null as Any?, x, null as Any?)) as Any?
    }
    if (x < ((tr as MutableMap<String, Any?>)["aa"] as Number).toDouble()) {
        return balance((node(((tr as MutableMap<String, Any?>)["cl"]) as String, ins(((tr as MutableMap<String, Any?>)["le"]) as Any?, x), ((tr as MutableMap<String, Any?>)["aa"]) as Int, ((tr as MutableMap<String, Any?>)["ri"]) as Any?)) as Any?)
    }
    if (x > ((tr as MutableMap<String, Any?>)["aa"] as Number).toDouble()) {
        return balance((node(((tr as MutableMap<String, Any?>)["cl"]) as String, ((tr as MutableMap<String, Any?>)["le"]) as Any?, ((tr as MutableMap<String, Any?>)["aa"]) as Int, ins(((tr as MutableMap<String, Any?>)["ri"]) as Any?, x))) as Any?)
    }
    return tr
}

fun insert(tr: Any?, x: Int): Any? {
    val t: Any? = ins(tr, x)
    if (t == null) {
        return null as Any?
    }
    val m: MutableMap<String, Any?> = t as MutableMap<String, Any?>
    return (node("B", (m)["le"] as Any?, ((m)["aa"] as Any?) as Int, (m)["ri"] as Any?)) as Any?
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        while (i <= 16) {
            tr = insert(tr, i)
            i = i + 1
        }
        println(treeString(tr))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
