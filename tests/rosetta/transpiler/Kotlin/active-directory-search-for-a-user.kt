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

fun search_user(directory: MutableMap<String, MutableList<String>>, username: String): MutableList<String> {
    return (directory)[username] as MutableList<String>
}

fun user_main(): Unit {
    val client: MutableMap<String, Any?> = mutableMapOf<String, Any?>("Base" to ("dc=example,dc=com"), "Host" to ("ldap.example.com"), "Port" to (389), "GroupFilter" to ("(memberUid=%s)"))
    val directory: MutableMap<String, MutableList<String>> = mutableMapOf<String, MutableList<String>>("username" to (mutableListOf("admins", "users")), "john" to (mutableListOf("users")))
    val groups: MutableList<String> = search_user(directory, "username")
    if (groups.size > 0) {
        var out: String = "Groups: ["
        var i: Int = 0
        while (i < groups.size) {
            out = ((out + "\"") + groups[i]) + "\""
            if (i < (groups.size - 1)) {
                out = out + ", "
            }
            i = i + 1
        }
        out = out + "]"
        println(out)
    } else {
        println("User not found")
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
