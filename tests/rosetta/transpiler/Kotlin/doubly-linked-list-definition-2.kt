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

fun newList(): MutableMap<String, Any?> {
    return mutableMapOf<String, Any?>("nodes" to (mutableMapOf<Any?, Any?>()), "head" to (0), "tail" to (0), "nextID" to (1))
}

fun newNode(l: MutableMap<String, Any?>, v: Any?): MutableMap<String, Any?> {
    val id: Int = (l)["nextID"] as Int
    (l)["nextID"] as Any? = (id + 1) as Any?
    var nodes: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
    val n: MutableMap<String, Int> = mutableMapOf<String, Any?>("id" to (id), "value" to (v), "next" to (0), "prev" to (0))
    (nodes)[id] as MutableMap<String, Any?> = n as MutableMap<String, Any?>
    (l)["nodes"] as Any? = nodes as Any?
    return n as MutableMap<String, Any?>
}

fun pushFront(l: MutableMap<String, Any?>, v: Any?): MutableMap<String, Any?> {
    var n: MutableMap<String, Any?> = newNode(l, v)
    (n)["next"] as Any? = (l)["head"] as Any?
    if ((l)["head"] as Int != 0) {
        var nodes: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
        var h: MutableMap<String, Any?>? = (nodes)[(l)["head"] as Int] as MutableMap<String, Any?>
        (h)["prev"] as Any? = (n)["id"] as Any?
        (nodes)[(h)["id"] as Int] as MutableMap<String, Any?> = h
        (l)["nodes"] as Any? = nodes as Any?
    } else {
        (l)["tail"] as Any? = (n)["id"] as Any?
    }
    (l)["head"] as Any? = (n)["id"] as Any?
    var nodes2: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
    (nodes2)[(n)["id"] as Int] as MutableMap<String, Any?> = n
    (l)["nodes"] as Any? = nodes2 as Any?
    return n
}

fun pushBack(l: MutableMap<String, Any?>, v: Any?): MutableMap<String, Any?> {
    var n: MutableMap<String, Any?> = newNode(l, v)
    (n)["prev"] as Any? = (l)["tail"] as Any?
    if ((l)["tail"] as Int != 0) {
        var nodes: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
        var t: MutableMap<String, Any?>? = (nodes)[(l)["tail"] as Int] as MutableMap<String, Any?>
        (t)["next"] as Any? = (n)["id"] as Any?
        (nodes)[(t)["id"] as Int] as MutableMap<String, Any?> = t
        (l)["nodes"] as Any? = nodes as Any?
    } else {
        (l)["head"] as Any? = (n)["id"] as Any?
    }
    (l)["tail"] as Any? = (n)["id"] as Any?
    var nodes2: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
    (nodes2)[(n)["id"] as Int] as MutableMap<String, Any?> = n
    (l)["nodes"] as Any? = nodes2 as Any?
    return n
}

fun insertBefore(l: MutableMap<String, Any?>, refID: Int, v: Any?): MutableMap<String, Any?> {
    if (refID == 0) {
        return pushFront(l, v)
    }
    var nodes: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
    var ref: MutableMap<String, Any?>? = (nodes)[refID] as MutableMap<String, Any?>
    var n: MutableMap<String, Any?> = newNode(l, v)
    (n)["prev"] as Any? = (ref)["prev"] as Any?
    (n)["next"] as Any? = (ref)["id"] as Any?
    if ((ref)["prev"] as Int != 0) {
        var p: MutableMap<String, Any?>? = (nodes)[(ref)["prev"] as Int] as MutableMap<String, Any?>
        (p)["next"] as Any? = (n)["id"] as Any?
        (nodes)[(p)["id"] as Int] as MutableMap<String, Any?> = p
    } else {
        (l)["head"] as Any? = (n)["id"] as Any?
    }
    (ref)["prev"] as Any? = (n)["id"] as Any?
    (nodes)[refID] as MutableMap<String, Any?> = ref
    (nodes)[(n)["id"] as Int] as MutableMap<String, Any?> = n
    (l)["nodes"] as Any? = nodes as Any?
    return n
}

fun insertAfter(l: MutableMap<String, Any?>, refID: Int, v: Any?): MutableMap<String, Any?> {
    if (refID == 0) {
        return pushBack(l, v)
    }
    var nodes: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
    var ref: MutableMap<String, Any?>? = (nodes)[refID] as MutableMap<String, Any?>
    var n: MutableMap<String, Any?> = newNode(l, v)
    (n)["next"] as Any? = (ref)["next"] as Any?
    (n)["prev"] as Any? = (ref)["id"] as Any?
    if ((ref)["next"] as Int != 0) {
        var nx: MutableMap<String, Any?>? = (nodes)[(ref)["next"] as Int] as MutableMap<String, Any?>
        (nx)["prev"] as Any? = (n)["id"] as Any?
        (nodes)[(nx)["id"] as Int] as MutableMap<String, Any?> = nx
    } else {
        (l)["tail"] as Any? = (n)["id"] as Any?
    }
    (ref)["next"] as Any? = (n)["id"] as Any?
    (nodes)[refID] as MutableMap<String, Any?> = ref
    (nodes)[(n)["id"] as Int] as MutableMap<String, Any?> = n
    (l)["nodes"] as Any? = nodes as Any?
    return n
}

fun user_main(): Unit {
    var l: MutableMap<String, Any?> = newList()
    var e4: MutableMap<String, Any?> = pushBack(l, 4 as Any?)
    var e1: MutableMap<String, Any?> = pushFront(l, 1 as Any?)
    insertBefore(l, (e4)["id"] as Int, 3 as Any?)
    insertAfter(l, (e1)["id"] as Int, "two" as Any?)
    var id: Int? = (l)["head"] as Int
    var nodes: MutableMap<Int, MutableMap<String, Any?>> = ((l)["nodes"] as Any?) as MutableMap<Int, MutableMap<String, Any?>>
    while (id != 0) {
        val node: MutableMap<String, Any?> = (nodes)[id] as MutableMap<String, Any?>
        println(((node)["value"] as Any?).toString())
        id = (node)["next"] as Int
    }
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
