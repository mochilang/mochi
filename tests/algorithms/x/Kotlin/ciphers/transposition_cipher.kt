fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun join_strings(xs: MutableList<String>): String {
    var res: String = ""
    var i: Int = 0
    while (i < xs.size) {
        res = res + xs[i]!!
        i = i + 1
    }
    return res
}

fun encrypt_message(key: Int, message: String): String {
    var result: String = ""
    var col: Int = 0
    while (col < key) {
        var pointer: Int = col
        while (pointer < message.length) {
            result = result + message.substring(pointer, pointer + 1)
            pointer = pointer + key
        }
        col = col + 1
    }
    return result
}

fun decrypt_message(key: Int, message: String): String {
    var num_cols: Int = ((message.length + key) - 1) / key
    var num_rows: Int = key
    var num_shaded_boxes: Int = (num_cols * num_rows) - message.length
    var plain_text: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < num_cols) {
        plain_text = run { val _tmp = plain_text.toMutableList(); _tmp.add(""); _tmp }
        i = i + 1
    }
    var col: Int = 0
    var row: Int = 0
    var index: Int = 0
    while (index < message.length) {
        _listSet(plain_text, col, plain_text[col]!! + message.substring(index, index + 1))
        col = col + 1
        if ((col == num_cols) || (((col == (num_cols - 1)) && (row >= (num_rows - num_shaded_boxes)) as Boolean))) {
            col = 0
            row = row + 1
        }
        index = index + 1
    }
    return join_strings(plain_text)
}

fun user_main(): Unit {
    println("Enter message: ")
    var message: String = input()
    var max_key: Int = message.length - 1
    println(("Enter key [2-" + max_key.toString()) + "]: ")
    var key: Int = ((input()).toInt())
    println("Encryption/Decryption [e/d]: ")
    var mode: String = input()
    var text: String = ""
    var first: String = mode.substring(0, 1)
    if ((first == "e") || (first == "E")) {
        text = encrypt_message(key, message)
    } else {
        if ((first == "d") || (first == "D")) {
            text = decrypt_message(key, message)
        }
    }
    println(("Output:\n" + text) + "|")
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
