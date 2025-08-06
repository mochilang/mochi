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

var BYTE_SIZE: Int = 256
fun pow_int(base: Int, exp: Int): Int {
    var result: Int = 1
    var i: Int = 0
    while (i < exp) {
        result = result * base
        i = i + 1
    }
    return result
}

fun mod_pow(base: Int, exponent: Int, modulus: Int): Int {
    var result: Int = 1
    var b: BigInteger = ((Math.floorMod(base, modulus)).toBigInteger())
    var e: Int = exponent
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = ((((result).toBigInteger().multiply((b).toBigInteger())).remainder((modulus).toBigInteger())).toInt())
        }
        e = e / 2
        b = (b.multiply((b).toBigInteger())).remainder((modulus).toBigInteger())
    }
    return result
}

fun ord(ch: String): Int {
    if (ch == " ") {
        return 32
    }
    if (ch == "a") {
        return 97
    }
    if (ch == "b") {
        return 98
    }
    if (ch == "c") {
        return 99
    }
    if (ch == "d") {
        return 100
    }
    if (ch == "e") {
        return 101
    }
    if (ch == "f") {
        return 102
    }
    if (ch == "g") {
        return 103
    }
    if (ch == "h") {
        return 104
    }
    if (ch == "i") {
        return 105
    }
    if (ch == "j") {
        return 106
    }
    if (ch == "k") {
        return 107
    }
    if (ch == "l") {
        return 108
    }
    if (ch == "m") {
        return 109
    }
    if (ch == "n") {
        return 110
    }
    if (ch == "o") {
        return 111
    }
    if (ch == "p") {
        return 112
    }
    if (ch == "q") {
        return 113
    }
    if (ch == "r") {
        return 114
    }
    if (ch == "s") {
        return 115
    }
    if (ch == "t") {
        return 116
    }
    if (ch == "u") {
        return 117
    }
    if (ch == "v") {
        return 118
    }
    if (ch == "w") {
        return 119
    }
    if (ch == "x") {
        return 120
    }
    if (ch == "y") {
        return 121
    }
    if (ch == "z") {
        return 122
    }
    return 0
}

fun chr(code: Int): String {
    if (code == 32) {
        return " "
    }
    if (code == 97) {
        return "a"
    }
    if (code == 98) {
        return "b"
    }
    if (code == 99) {
        return "c"
    }
    if (code == 100) {
        return "d"
    }
    if (code == 101) {
        return "e"
    }
    if (code == 102) {
        return "f"
    }
    if (code == 103) {
        return "g"
    }
    if (code == 104) {
        return "h"
    }
    if (code == 105) {
        return "i"
    }
    if (code == 106) {
        return "j"
    }
    if (code == 107) {
        return "k"
    }
    if (code == 108) {
        return "l"
    }
    if (code == 109) {
        return "m"
    }
    if (code == 110) {
        return "n"
    }
    if (code == 111) {
        return "o"
    }
    if (code == 112) {
        return "p"
    }
    if (code == 113) {
        return "q"
    }
    if (code == 114) {
        return "r"
    }
    if (code == 115) {
        return "s"
    }
    if (code == 116) {
        return "t"
    }
    if (code == 117) {
        return "u"
    }
    if (code == 118) {
        return "v"
    }
    if (code == 119) {
        return "w"
    }
    if (code == 120) {
        return "x"
    }
    if (code == 121) {
        return "y"
    }
    if (code == 122) {
        return "z"
    }
    return ""
}

fun get_blocks_from_text(message: String, block_size: Int): MutableList<Int> {
    var block_ints: MutableList<Int> = mutableListOf<Int>()
    var block_start: Int = 0
    while (block_start < message.length) {
        var block_int: Int = 0
        var i: Int = block_start
        while ((i < (block_start + block_size)) && (i < message.length)) {
            block_int = block_int + (ord(message[i].toString()) * pow_int(BYTE_SIZE, i - block_start))
            i = i + 1
        }
        block_ints = run { val _tmp = block_ints.toMutableList(); _tmp.add(block_int); _tmp }
        block_start = block_start + block_size
    }
    return block_ints
}

fun get_text_from_blocks(block_ints: MutableList<Int>, message_length: Int, block_size: Int): String {
    var message: String = ""
    for (block_int in block_ints) {
        var block: Int = block_int
        var i: BigInteger = ((block_size - 1).toBigInteger())
        var block_message: String = ""
        while (i.compareTo((0).toBigInteger()) >= 0) {
            if (((message.length).toBigInteger().add((i).toBigInteger())).compareTo((message_length).toBigInteger()) < 0) {
                var ascii_number: Int = block / pow_int(BYTE_SIZE, (i.toInt()))
                block = Math.floorMod(block, pow_int(BYTE_SIZE, (i.toInt())))
                block_message = chr(ascii_number) + block_message
            }
            i = i.subtract((1).toBigInteger())
        }
        message = message + block_message
    }
    return message
}

fun encrypt_message(message: String, n: Int, e: Int, block_size: Int): MutableList<Int> {
    var encrypted: MutableList<Int> = mutableListOf<Int>()
    var blocks: MutableList<Int> = get_blocks_from_text(message, block_size)
    for (block in blocks) {
        encrypted = run { val _tmp = encrypted.toMutableList(); _tmp.add(mod_pow(block, e, n)); _tmp }
    }
    return encrypted
}

fun decrypt_message(blocks: MutableList<Int>, message_length: Int, n: Int, d: Int, block_size: Int): String {
    var decrypted_blocks: MutableList<Int> = mutableListOf<Int>()
    for (block in blocks) {
        decrypted_blocks = run { val _tmp = decrypted_blocks.toMutableList(); _tmp.add(mod_pow(block, d, n)); _tmp }
    }
    var message: String = ""
    for (num in decrypted_blocks) {
        message = message + chr(num)
    }
    return message
}

fun user_main(): Unit {
    var message: String = "hello world"
    var n: Int = 3233
    var e: Int = 17
    var d: Int = 2753
    var block_size: Int = 1
    var encrypted: MutableList<Int> = encrypt_message(message, n, e, block_size)
    println(encrypted.toString())
    var decrypted: String = decrypt_message(encrypted, message.length, n, d, block_size)
    println(decrypted)
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
