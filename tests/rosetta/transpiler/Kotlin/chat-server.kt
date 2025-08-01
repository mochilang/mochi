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

fun removeName(names: MutableList<String>, name: String): MutableList<String> {
    var out: MutableList<String> = mutableListOf<String>()
    for (n in names) {
        if (n != name) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(n); _tmp } as MutableList<String>
        }
    }
    return out
}

fun user_main(): Unit {
    var clients: MutableList<String> = mutableListOf<String>()
    fun broadcast(msg: String): Unit {
        println(msg)
    }

    fun add(name: String): Unit {
        clients = run { val _tmp = clients.toMutableList(); _tmp.add(name); _tmp } as MutableList<String>
        broadcast(("+++ \"" + name) + "\" connected +++\n")
    }

    fun send(name: String, msg: String): Unit {
        broadcast(((name + "> ") + msg) + "\n")
    }

    fun remove(name: String): Unit {
        clients = removeName(clients, name)
        broadcast(("--- \"" + name) + "\" disconnected ---\n")
    }

    add("Alice")
    add("Bob")
    send("Alice", "Hello Bob!")
    send("Bob", "Hi Alice!")
    remove("Bob")
    remove("Alice")
    broadcast("Server stopping!\n")
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
