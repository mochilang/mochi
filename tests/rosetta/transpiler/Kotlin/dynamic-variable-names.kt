fun input(): String = readLine() ?: ""

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

fun parseIntStr(str: String): Int {
    var i: Int = 0
    var neg: Boolean = false
    if ((str.length > 0) && (str.substring(0, 1) == "-")) {
        neg = true
        i = 1
    }
    var n: Int = 0
    var digits: MutableMap<String, Int> = mutableMapOf<String, Int>("0" to (0), "1" to (1), "2" to (2), "3" to (3), "4" to (4), "5" to (5), "6" to (6), "7" to (7), "8" to (8), "9" to (9))
    while (i < str.length) {
        n = (n * 10) + (digits)[str.substring(i, i + 1)] as Int
        i = i + 1
    }
    if ((neg as Boolean)) {
        n = 0 - n
    }
    return n
}

fun user_main(): Unit {
    var n: Int = 0
    while ((n < 1) || (n > 5)) {
        println("How many integer variables do you want to create (max 5) : ")
        var line: String = input()
        if (line.length > 0) {
            n = Integer.parseInt(line, 10)
        }
    }
    var vars: MutableMap<String, Int> = mutableMapOf<String, Int>()
    println("OK, enter the variable names and their values, below\n")
    var i: Int = 1
    while (i <= n) {
        println(("\n  Variable " + i.toString()) + "\n")
        println("    Name  : ")
        var name: String = input()
        if (name in vars) {
            println("  Sorry, you've already created a variable of that name, try again")
            continue
        }
        var value: Int = 0
        while (true) {
            println("    Value : ")
            var valstr: String = input()
            if (valstr.length == 0) {
                println("  Not a valid integer, try again")
                continue
            }
            var ok: Boolean = true
            var j: Int = 0
            var neg: Boolean = false
            if (valstr.substring(0, 1) == "-") {
                neg = true
                j = 1
            }
            while (j < valstr.length) {
                var ch: String = valstr.substring(j, j + 1)
                if ((ch < "0") || (ch > "9")) {
                    ok = false
                    break
                }
                j = j + 1
            }
            if (!ok) {
                println("  Not a valid integer, try again")
                continue
            }
            value = Integer.parseInt(valstr, 10)
            break
        }
        (vars)[name] = value
        i = i + 1
    }
    println("\nEnter q to quit")
    while (true) {
        println("\nWhich variable do you want to inspect : ")
        var name: String = input()
        if (name.toLowerCase() == "q") {
            return
        }
        if (name in vars) {
            println("It's value is " + ((vars)[name] as Int).toString())
        } else {
            println("Sorry there's no variable of that name, try again")
        }
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
