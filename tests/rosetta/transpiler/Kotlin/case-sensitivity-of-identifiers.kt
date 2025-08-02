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

fun user_main(): Unit {
    var pkg_dog: String = "Salt"
    var Dog: String = "Pepper"
    var pkg_DOG: String = "Mustard"
    fun packageSees(d1: String, d2: String, d3: String): MutableMap<String, Boolean> {
        println((((("Package sees: " + d1) + " ") + d2) + " ") + d3)
        return mutableMapOf<String, Boolean>("pkg_dog" to (true), "Dog" to (true), "pkg_DOG" to (true))
    }

    var d: MutableMap<String, Boolean> = packageSees(pkg_dog, Dog, pkg_DOG)
    println(("There are " + d.size.toString()) + " dogs.\n")
    var dog: String = "Benjamin"
    d = (packageSees(pkg_dog, Dog, pkg_DOG)) as MutableMap<String, Boolean>
    println((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG)
    (d)["dog"] = true
    (d)["Dog"] = true
    (d)["pkg_DOG"] = true
    println(("There are " + d.size.toString()) + " dogs.\n")
    Dog = "Samba"
    d = (packageSees(pkg_dog, Dog, pkg_DOG)) as MutableMap<String, Boolean>
    println((((("Main sees:   " + dog) + " ") + Dog) + " ") + pkg_DOG)
    (d)["dog"] = true
    (d)["Dog"] = true
    (d)["pkg_DOG"] = true
    println(("There are " + d.size.toString()) + " dogs.\n")
    var DOG: String = "Bernie"
    d = (packageSees(pkg_dog, Dog, pkg_DOG)) as MutableMap<String, Boolean>
    println((((("Main sees:   " + dog) + " ") + Dog) + " ") + DOG)
    (d)["dog"] = true
    (d)["Dog"] = true
    (d)["pkg_DOG"] = true
    (d)["DOG"] = true
    println(("There are " + d.size.toString()) + " dogs.")
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
