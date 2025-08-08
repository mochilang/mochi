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

var triagrams: MutableList<String> = mutableListOf("111", "112", "113", "121", "122", "123", "131", "132", "133", "211", "212", "213", "221", "222", "223", "231", "232", "233", "311", "312", "313", "321", "322", "323", "331", "332", "333")
fun remove_spaces(s: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        if (c != " ") {
            res = res + c
        }
        i = i + 1
    }
    return res
}

fun char_to_trigram(ch: String, alphabet: String): String {
    var i: Int = 0
    while (i < alphabet.length) {
        if (alphabet.substring(i, i + 1) == ch) {
            return triagrams[i]!!
        }
        i = i + 1
    }
    return ""
}

fun trigram_to_char(tri: String, alphabet: String): String {
    var i: Int = 0
    while (i < triagrams.size) {
        if (triagrams[i]!! == tri) {
            return alphabet.substring(i, i + 1)
        }
        i = i + 1
    }
    return ""
}

fun encrypt_part(part: String, alphabet: String): String {
    var one: String = ""
    var two: String = ""
    var three: String = ""
    var i: Int = 0
    while (i < part.length) {
        var tri: String = char_to_trigram(part.substring(i, i + 1), alphabet)
        one = one + tri.substring(0, 1)
        two = two + tri.substring(1, 2)
        three = three + tri.substring(2, 3)
        i = i + 1
    }
    return (one + two) + three
}

fun encrypt_message(message: String, alphabet: String, period: Int): String {
    var msg: String = remove_spaces(message)
    var alpha: String = remove_spaces(alphabet)
    if (alpha.length != 27) {
        return ""
    }
    var encrypted_numeric: String = ""
    var i: Int = 0
    while (i < msg.length) {
        var end: BigInteger = ((i + period).toBigInteger())
        if (end.compareTo((msg.length).toBigInteger()) > 0) {
            end = (msg.length.toBigInteger())
        }
        var part: String = msg.substring(i, (end).toInt())
        encrypted_numeric = encrypted_numeric + encrypt_part(part, alpha)
        i = i + period
    }
    var encrypted: String = ""
    var j: Int = 0
    while (j < encrypted_numeric.length) {
        var tri: String = encrypted_numeric.substring(j, j + 3)
        encrypted = encrypted + trigram_to_char(tri, alpha)
        j = j + 3
    }
    return encrypted
}

fun decrypt_part(part: String, alphabet: String): MutableList<String> {
    var converted: String = ""
    var i: Int = 0
    while (i < part.length) {
        var tri: String = char_to_trigram(part.substring(i, i + 1), alphabet)
        converted = converted + tri
        i = i + 1
    }
    var result: MutableList<String> = mutableListOf<String>()
    var tmp: String = ""
    var j: Int = 0
    while (j < converted.length) {
        tmp = tmp + converted.substring(j, j + 1)
        if (tmp.length == part.length) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(tmp); _tmp }
            tmp = ""
        }
        j = j + 1
    }
    return result
}

fun decrypt_message(message: String, alphabet: String, period: Int): String {
    var msg: String = remove_spaces(message)
    var alpha: String = remove_spaces(alphabet)
    if (alpha.length != 27) {
        return ""
    }
    var decrypted_numeric: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < msg.length) {
        var end: BigInteger = ((i + period).toBigInteger())
        if (end.compareTo((msg.length).toBigInteger()) > 0) {
            end = (msg.length.toBigInteger())
        }
        var part: String = msg.substring(i, (end).toInt())
        var groups: MutableList<String> = decrypt_part(part, alpha)
        var k: Int = 0
        while (k < (groups[0]!!).length) {
            var tri: String = (groups[0]!!.substring(k, k + 1) + groups[1]!!.substring(k, k + 1)) + groups[2]!!.substring(k, k + 1)
            decrypted_numeric = run { val _tmp = decrypted_numeric.toMutableList(); _tmp.add(tri); _tmp }
            k = k + 1
        }
        i = i + period
    }
    var decrypted: String = ""
    var j: Int = 0
    while (j < decrypted_numeric.size) {
        decrypted = decrypted + trigram_to_char(decrypted_numeric[j]!!, alpha)
        j = j + 1
    }
    return decrypted
}

fun user_main(): Unit {
    var msg: String = "DEFEND THE EAST WALL OF THE CASTLE."
    var alphabet: String = "EPSDUCVWYM.ZLKXNBTFGORIJHAQ"
    var encrypted: String = encrypt_message(msg, alphabet, 5)
    var decrypted: String = decrypt_message(encrypted, alphabet, 5)
    println("Encrypted: " + encrypted)
    println("Decrypted: " + decrypted)
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
