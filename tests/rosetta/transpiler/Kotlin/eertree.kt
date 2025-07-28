import java.math.BigInteger

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
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

val EVEN_ROOT: Int = 0
val ODD_ROOT: Int = 1
fun newNode(len: Int): MutableMap<String, Any?> {
    return mutableMapOf<String, Any?>("length" to (len), "edges" to (mutableMapOf<Any?, Any?>()), "suffix" to (0))
}

fun eertree(s: String): MutableList<MutableMap<String, Any?>> {
    var tree: MutableList<MutableMap<String, Any?>> = mutableListOf<MutableMap<String, Any?>>()
    tree = run { val _tmp = tree.toMutableList(); _tmp.add(mutableMapOf<String, Any?>("length" to (0), "suffix" to (ODD_ROOT), "edges" to (mutableMapOf<Any?, Any?>()))); _tmp } as MutableList<MutableMap<String, Any?>>
    tree = run { val _tmp = tree.toMutableList(); _tmp.add(mutableMapOf<String, Any?>("length" to (0 - 1), "suffix" to (ODD_ROOT), "edges" to (mutableMapOf<Any?, Any?>()))); _tmp } as MutableList<MutableMap<String, Any?>>
    var suffix: Int = ODD_ROOT
    var i: Int = 0
    while (i < s.length) {
        val c: String = s.substring(i, i + 1)
        var n: Int = suffix
        var k: Int = 0
        while (true) {
            k = (tree[n])["length"] as Int
            val b: BigInteger = ((i - k) - 1).toBigInteger()
            if ((b.compareTo(0.toBigInteger()) >= 0) && (s.substring((b).toInt(), (b.add(1.toBigInteger())).toInt()) == c)) {
                break
            }
            n = (tree[n])["suffix"] as Int
        }
        var edges: MutableMap<String, Int> = ((tree[n])["edges"] as Any?) as MutableMap<String, Int>
        if (c in edges) {
            suffix = (edges)[c] as Int
            i = i + 1
            continue
        }
        suffix = tree.size
        tree = run { val _tmp = tree.toMutableList(); _tmp.add(newNode(k + 2)); _tmp } as MutableList<MutableMap<String, Any?>>
        (edges)[c] as Int = suffix
        (tree[n])["edges"] as Any? = edges as Any?
        if ((tree[suffix])["length"] as Int == 1) {
            (tree[suffix])["suffix"] as Any? = 0 as Any?
            i = i + 1
            continue
        }
        while (true) {
            n = (tree[n])["suffix"] as Int
            val b: BigInteger = ((i - (tree[n])["length"] as Int) - 1).toBigInteger()
            if ((b.compareTo(0.toBigInteger()) >= 0) && (s.substring((b).toInt(), (b.add(1.toBigInteger())).toInt()) == c)) {
                break
            }
        }
        var en: MutableMap<String, Int> = ((tree[n])["edges"] as Any?) as MutableMap<String, Int>
        (tree[suffix])["suffix"] as Any? = ((en)[c] as Int) as Any?
        i = i + 1
    }
    return tree
}

fun child(tree: MutableList<MutableMap<String, Any?>>, idx: Int, p: String, acc: MutableList<String>): MutableList<String> {
    var acc: MutableList<String> = acc
    var edges: MutableMap<String, Int> = ((tree[idx])["edges"] as Any?) as MutableMap<String, Int>
    for (ch in edges.keys) {
        val nxt: Int = (edges)[ch] as Int
        val pal: String = ((ch).toString() + p) + (ch).toString()
        acc = run { val _tmp = acc.toMutableList(); _tmp.add(pal); _tmp } as MutableList<String>
        acc = child(tree, nxt, pal, acc)
    }
    return acc
}

fun subPalindromes(tree: MutableList<MutableMap<String, Any?>>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    res = child(tree, EVEN_ROOT, "", res)
    var oEdges: MutableMap<String, Int> = ((tree[ODD_ROOT])["edges"] as Any?) as MutableMap<String, Int>
    for (ch in oEdges.keys) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(ch as String); _tmp } as MutableList<String>
        res = child(tree, (oEdges)[ch] as Int, ch as String, res)
    }
    return res
}

fun user_main(): Unit {
    val tree: MutableList<MutableMap<String, Any?>> = eertree("eertree")
    val subs: MutableList<String> = subPalindromes(tree)
    println(subs.toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
