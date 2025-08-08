import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var key: Int = 6
var message: String = "Harshil Darji"
var encrypted: String = encrypt_message(key, message)
fun encrypt_message(key: Int, message: String): String {
    var result: String = ""
    var col: Int = 0
    while (col < key) {
        var pointer: Int = col
        while (pointer < message.length) {
            result = result + message[pointer].toString()
            pointer = pointer + key
        }
        col = col + 1
    }
    return result
}

fun decrypt_message(key: Int, message: String): String {
    var msg_len: Int = message.length
    var num_cols: Int = msg_len / key
    if ((Math.floorMod(msg_len, key)) != 0) {
        num_cols = num_cols + 1
    }
    var num_rows: Int = key
    var num_shaded_boxes: Int = (num_cols * num_rows) - msg_len
    var plain: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < num_cols) {
        plain = run { val _tmp = plain.toMutableList(); _tmp.add(""); _tmp }
        i = i + 1
    }
    var col: Int = 0
    var row: Int = 0
    var idx: Int = 0
    while (idx < msg_len) {
        var ch: String = message[idx].toString()
        _listSet(plain, col, plain[col]!! + ch)
        col = col + 1
        if ((col == num_cols) || (((col == (num_cols - 1)) && (row >= (num_rows - num_shaded_boxes)) as Boolean))) {
            col = 0
            row = row + 1
        }
        idx = idx + 1
    }
    var result: String = ""
    i = 0
    while (i < num_cols) {
        result = result + plain[i]!!
        i = i + 1
    }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(encrypted)
        var decrypted: String = decrypt_message(key, encrypted)
        println(decrypted)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
