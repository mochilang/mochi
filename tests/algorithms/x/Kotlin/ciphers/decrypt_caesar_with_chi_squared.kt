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

data class Result(var shift: Int = 0, var chi: Double = 0.0, var decoded: String = "")
var r1: Result = decrypt_caesar_with_chi_squared("dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!", mutableListOf<String>(), mutableMapOf<String, Double>(), false)
fun default_alphabet(): MutableList<String> {
    return mutableListOf("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z")
}

fun default_frequencies(): MutableMap<String, Double> {
    return mutableMapOf<String, Double>("a" to (0.08497), "b" to (0.01492), "c" to (0.02202), "d" to (0.04253), "e" to (0.11162), "f" to (0.02228), "g" to (0.02015), "h" to (0.06094), "i" to (0.07546), "j" to (0.00153), "k" to (0.01292), "l" to (0.04025), "m" to (0.02406), "n" to (0.06749), "o" to (0.07507), "p" to (0.01929), "q" to (0.00095), "r" to (0.07587), "s" to (0.06327), "t" to (0.09356), "u" to (0.02758), "v" to (0.00978), "w" to (0.0256), "x" to (0.0015), "y" to (0.01994), "z" to (0.00077))
}

fun index_of(xs: MutableList<String>, ch: String): Int {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun count_char(s: String, ch: String): Int {
    var count: Int = 0
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            count = count + 1
        }
        i = i + 1
    }
    return count
}

fun decrypt_caesar_with_chi_squared(ciphertext: String, cipher_alphabet: MutableList<String>, frequencies_dict: MutableMap<String, Double>, case_sensitive: Boolean): Result {
    var ciphertext: String = ciphertext
    var alphabet_letters: MutableList<String> = cipher_alphabet
    if (alphabet_letters.size == 0) {
        alphabet_letters = default_alphabet()
    }
    var frequencies: MutableMap<String, Double> = frequencies_dict
    if (frequencies.size == 0) {
        frequencies = default_frequencies()
    }
    if (!case_sensitive) {
        ciphertext = (ciphertext.toLowerCase() as String)
    }
    var best_shift: Int = 0
    var best_chi: Double = 0.0
    var best_text: String = ""
    var shift: Int = 0
    while (shift < alphabet_letters.size) {
        var decrypted: String = ""
        var i: Int = 0
        while (i < ciphertext.length) {
            var ch: String = ciphertext.substring(i, i + 1)
            var idx: Int = index_of(alphabet_letters, (ch.toLowerCase() as String))
            if (idx >= 0) {
                var m: Int = alphabet_letters.size
                var new_idx: BigInteger = ((Math.floorMod((idx - shift), m)).toBigInteger())
                if (new_idx.compareTo((0).toBigInteger()) < 0) {
                    new_idx = new_idx.add((m).toBigInteger())
                }
                var new_char: String = alphabet_letters[(new_idx).toInt()]!!
                if (case_sensitive && (ch != ch.toLowerCase())) {
                    decrypted = decrypted + (new_char.toUpperCase()).toString()
                } else {
                    decrypted = decrypted + new_char
                }
            } else {
                decrypted = decrypted + ch
            }
            i = i + 1
        }
        var chi: Double = 0.0
        var lowered: String = (if (case_sensitive != null) decrypted.toLowerCase() else decrypted as String)
        var j: Int = 0
        while (j < alphabet_letters.size) {
            var letter: String = alphabet_letters[j]!!
            var occ: Int = count_char(lowered, letter)
            if (occ > 0) {
                var occf: Double = (occ.toDouble())
                var expected: Double = (frequencies)[letter] as Double * occf
                var diff: Double = occf - expected
                chi = chi + (((diff * diff) / expected) * occf)
            }
            j = j + 1
        }
        if ((shift == 0) || (chi < best_chi)) {
            best_shift = shift
            best_chi = chi
            best_text = decrypted
        }
        shift = shift + 1
    }
    return Result(shift = best_shift, chi = best_chi, decoded = best_text)
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println((((r1.shift.toString() + ", ") + r1.chi.toString()) + ", ") + r1.decoded)
        var r2: Result = decrypt_caesar_with_chi_squared("crybd cdbsxq", mutableListOf<String>(), mutableMapOf<String, Double>(), false)
        println((((r2.shift.toString() + ", ") + r2.chi.toString()) + ", ") + r2.decoded)
        var r3: Result = decrypt_caesar_with_chi_squared("Crybd Cdbsxq", mutableListOf<String>(), mutableMapOf<String, Double>(), true)
        println((((r3.shift.toString() + ", ") + r3.chi.toString()) + ", ") + r3.decoded)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
