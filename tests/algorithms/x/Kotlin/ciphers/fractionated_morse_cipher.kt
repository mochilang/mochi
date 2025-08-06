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

var MORSE_CODE_DICT: MutableMap<String, String> = mutableMapOf<String, String>("A" to (".-"), "B" to ("-..."), "C" to ("-.-."), "D" to ("-.."), "E" to ("."), "F" to ("..-."), "G" to ("--."), "H" to ("...."), "I" to (".."), "J" to (".---"), "K" to ("-.-"), "L" to (".-.."), "M" to ("--"), "N" to ("-."), "O" to ("---"), "P" to (".--."), "Q" to ("--.-"), "R" to (".-."), "S" to ("..."), "T" to ("-"), "U" to ("..-"), "V" to ("...-"), "W" to (".--"), "X" to ("-..-"), "Y" to ("-.--"), "Z" to ("--.."), " " to (""))
var MORSE_COMBINATIONS: MutableList<String> = mutableListOf("...", "..-", "..x", ".-.", ".--", ".-x", ".x.", ".x-", ".xx", "-..", "-.-", "-.x", "--.", "---", "--x", "-x.", "-x-", "-xx", "x..", "x.-", "x.x", "x-.", "x--", "x-x", "xx.", "xx-", "xxx")
var REVERSE_DICT: MutableMap<String, String> = mutableMapOf<String, String>(".-" to ("A"), "-..." to ("B"), "-.-." to ("C"), "-.." to ("D"), "." to ("E"), "..-." to ("F"), "--." to ("G"), "...." to ("H"), ".." to ("I"), ".---" to ("J"), "-.-" to ("K"), ".-.." to ("L"), "--" to ("M"), "-." to ("N"), "---" to ("O"), ".--." to ("P"), "--.-" to ("Q"), ".-." to ("R"), "..." to ("S"), "-" to ("T"), "..-" to ("U"), "...-" to ("V"), ".--" to ("W"), "-..-" to ("X"), "-.--" to ("Y"), "--.." to ("Z"), "" to (" "))
var plaintext: String = "defend the east"
fun encodeToMorse(plaintext: String): String {
    var morse: String = ""
    var i: Int = 0
    while (i < plaintext.length) {
        var ch: String = (plaintext.substring(i, i + 1).toUpperCase() as String)
        var code: String = ""
        if (ch in MORSE_CODE_DICT) {
            code = (MORSE_CODE_DICT)[ch] as String
        }
        if (i > 0) {
            morse = morse + "x"
        }
        morse = morse + code
        i = i + 1
    }
    return morse
}

fun encryptFractionatedMorse(plaintext: String, key: String): String {
    var morseCode: String = encodeToMorse(plaintext)
    var combinedKey: String = (key.toUpperCase()).toString() + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var dedupKey: String = ""
    var i: Int = 0
    while (i < combinedKey.length) {
        var ch: String = combinedKey.substring(i, i + 1)
        if (!(ch in dedupKey)) {
            dedupKey = dedupKey + ch
        }
        i = i + 1
    }
    var paddingLength: BigInteger = ((3 - (Math.floorMod(morseCode.length, 3))).toBigInteger())
    var p: Int = 0
    while ((p).toBigInteger().compareTo((paddingLength)) < 0) {
        morseCode = morseCode + "x"
        p = p + 1
    }
    var dict: MutableMap<String, String> = mutableMapOf<String, String>()
    var j: Int = 0
    while (j < 26) {
        var combo: String = MORSE_COMBINATIONS[j]!!
        var letter: String = dedupKey.substring(j, j + 1)
        (dict)[combo] = letter
        j = j + 1
    }
    (dict)["xxx"] = ""
    var encrypted: String = ""
    var k: Int = 0
    while (k < morseCode.length) {
        var group: String = morseCode.substring(k, k + 3)
        encrypted = encrypted + (dict)[group] as String
        k = k + 3
    }
    return encrypted
}

fun decryptFractionatedMorse(ciphertext: String, key: String): String {
    var combinedKey: String = (key.toUpperCase()).toString() + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var dedupKey: String = ""
    var i: Int = 0
    while (i < combinedKey.length) {
        var ch: String = combinedKey.substring(i, i + 1)
        if (!(ch in dedupKey)) {
            dedupKey = dedupKey + ch
        }
        i = i + 1
    }
    var inv: MutableMap<String, String> = mutableMapOf<String, String>()
    var j: Int = 0
    while (j < 26) {
        var letter: String = dedupKey.substring(j, j + 1)
        (inv)[letter] = MORSE_COMBINATIONS[j]!!
        j = j + 1
    }
    var morse: String = ""
    var k: Int = 0
    while (k < ciphertext.length) {
        var ch: String = ciphertext.substring(k, k + 1)
        if (ch in inv) {
            morse = morse + (inv)[ch] as String
        }
        k = k + 1
    }
    var codes: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var m: Int = 0
    while (m < morse.length) {
        var ch: String = morse.substring(m, m + 1)
        if (ch == "x") {
            codes = run { val _tmp = codes.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            current = current + ch
        }
        m = m + 1
    }
    codes = run { val _tmp = codes.toMutableList(); _tmp.add(current); _tmp }
    var decrypted: String = ""
    var idx: Int = 0
    while (idx < codes.size) {
        var code: String = codes[idx]!!
        decrypted = decrypted + (REVERSE_DICT)[code] as String
        idx = idx + 1
    }
    var start: Int = 0
    while (true) {
        if (start < decrypted.length) {
            if (decrypted.substring(start, start + 1) == " ") {
                start = start + 1
                continue
            }
        }
        break
    }
    var end: Int = decrypted.length
    while (true) {
        if (end > start) {
            if (decrypted.substring(end - 1, end) == " ") {
                end = end - 1
                continue
            }
        }
        break
    }
    return decrypted.substring(start, end)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(listOf("Plain Text:", plaintext).joinToString(" "))
        var key: String = "ROUNDTABLE"
        var ciphertext: String = encryptFractionatedMorse(plaintext, key)
        println(listOf("Encrypted:", ciphertext).joinToString(" "))
        var decrypted: String = decryptFractionatedMorse(ciphertext, key)
        println(listOf("Decrypted:", decrypted).joinToString(" "))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
