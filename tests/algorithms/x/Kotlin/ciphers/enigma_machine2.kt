import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var abc: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var low_abc: String = "abcdefghijklmnopqrstuvwxyz"
var rotor1: String = "EGZWVONAHDCLFQMSIPJBYUKXTR"
var rotor2: String = "FOBHMDKEXQNRAULPGSJVTYICZW"
var rotor3: String = "ZJXESIUQLHAVRMDOYGTNFWPBKC"
var rotor4: String = "RMDJXFUWGISLHVTCQNKYPBEZOA"
var rotor5: String = "SGLCPQWZHKXAREONTFBVIYJUDM"
var rotor6: String = "HVSICLTYKQUBXDWAJZOMFGPREN"
var rotor7: String = "RZWQHFMVDBKICJLNTUXAGYPSOE"
var rotor8: String = "LFKIJODBEGAMQPXVUHYSTCZRWN"
var rotor9: String = "KOAEGVDHXPQZMLFTYWJNBRCIUS"
var reflector_pairs: MutableList<String> = mutableListOf("AN", "BO", "CP", "DQ", "ER", "FS", "GT", "HU", "IV", "JW", "KX", "LY", "MZ")
fun list_contains(xs: MutableList<String>, x: String): Boolean {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun index_in_string(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun contains_char(s: String, ch: String): Boolean {
    return index_in_string(s, ch) >= 0
}

fun to_uppercase(s: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        var idx: Int = index_in_string(low_abc, ch)
        if (idx >= 0) {
            res = res + abc.substring(idx, idx + 1)
        } else {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun plugboard_map(pb: MutableList<String>, ch: String): String {
    var i: Int = 0
    while (i < pb.size) {
        var pair: String = pb[i]!!
        var a: String = pair.substring(0, 1)
        var b: String = pair.substring(1, 2)
        if (ch == a) {
            return b
        }
        if (ch == b) {
            return a
        }
        i = i + 1
    }
    return ch
}

fun reflector_map(ch: String): String {
    var i: Int = 0
    while (i < reflector_pairs.size) {
        var pair: String = reflector_pairs[i]!!
        var a: String = pair.substring(0, 1)
        var b: String = pair.substring(1, 2)
        if (ch == a) {
            return b
        }
        if (ch == b) {
            return a
        }
        i = i + 1
    }
    return ch
}

fun count_unique(xs: MutableList<String>): Int {
    var unique: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < xs.size) {
        if (!list_contains(unique, xs[i]!!)) {
            unique = run { val _tmp = unique.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return unique.size
}

fun build_plugboard(pbstring: String): MutableList<String> {
    if (pbstring.length == 0) {
        return mutableListOf<String>()
    }
    if ((Math.floorMod(pbstring.length, 2)) != 0) {
        panic(("Odd number of symbols(" + pbstring.length.toString()) + ")")
    }
    var pbstring_nospace: String = ""
    var i: Int = 0
    while (i < pbstring.length) {
        var ch: String = pbstring.substring(i, i + 1)
        if (ch != " ") {
            pbstring_nospace = pbstring_nospace + ch
        }
        i = i + 1
    }
    var seen: MutableList<String> = mutableListOf<String>()
    i = 0
    while (i < pbstring_nospace.length) {
        var ch: String = pbstring_nospace.substring(i, i + 1)
        if (!contains_char(abc, ch)) {
            panic(("'" + ch) + "' not in list of symbols")
        }
        if (((list_contains(seen, ch)) as Boolean)) {
            panic(("Duplicate symbol(" + ch) + ")")
        }
        seen = run { val _tmp = seen.toMutableList(); _tmp.add(ch); _tmp }
        i = i + 1
    }
    var pb: MutableList<String> = mutableListOf<String>()
    i = 0
    while (i < (pbstring_nospace.length - 1)) {
        var a: String = pbstring_nospace.substring(i, i + 1)
        var b: String = pbstring_nospace.substring(i + 1, i + 2)
        pb = run { val _tmp = pb.toMutableList(); _tmp.add(a + b); _tmp }
        i = i + 2
    }
    return pb
}

fun validator(rotpos: MutableList<Int>, rotsel: MutableList<String>, pb: String): Unit {
    if (count_unique(rotsel) < 3) {
        panic(("Please use 3 unique rotors (not " + count_unique(rotsel).toString()) + ")")
    }
    if (rotpos.size != 3) {
        panic("Rotor position must have 3 values")
    }
    var r1: Int = rotpos[0]!!
    var r2: Int = rotpos[1]!!
    var r3: Int = rotpos[2]!!
    if (!(((0 < r1) && (r1 <= abc.length)) as Boolean)) {
        panic(("First rotor position is not within range of 1..26 (" + r1.toString()) + ")")
    }
    if (!(((0 < r2) && (r2 <= abc.length)) as Boolean)) {
        panic(("Second rotor position is not within range of 1..26 (" + r2.toString()) + ")")
    }
    if (!(((0 < r3) && (r3 <= abc.length)) as Boolean)) {
        panic(("Third rotor position is not within range of 1..26 (" + r3.toString()) + ")")
    }
}

fun enigma(text: String, rotor_position: MutableList<Int>, rotor_selection: MutableList<String>, plugb: String): String {
    var up_text: String = to_uppercase(text)
    var up_pb: String = to_uppercase(plugb)
    validator(rotor_position, rotor_selection, up_pb)
    var plugboard: MutableList<String> = build_plugboard(up_pb)
    var rotorpos1: BigInteger = ((rotor_position[0]!! - 1).toBigInteger())
    var rotorpos2: BigInteger = ((rotor_position[1]!! - 1).toBigInteger())
    var rotorpos3: BigInteger = ((rotor_position[2]!! - 1).toBigInteger())
    var rotor_a: String = rotor_selection[0]!!
    var rotor_b: String = rotor_selection[1]!!
    var rotor_c: String = rotor_selection[2]!!
    var result: String = ""
    var i: Int = 0
    while (i < up_text.length) {
        var symbol: String = up_text.substring(i, i + 1)
        if (((contains_char(abc, symbol)) as Boolean)) {
            symbol = plugboard_map(plugboard, symbol)
            var index: BigInteger = (index_in_string(abc, symbol)).toBigInteger().add((rotorpos1))
            symbol = rotor_a.substring((index.remainder((abc.length).toBigInteger())).toInt(), ((index.remainder((abc.length).toBigInteger())).add((1).toBigInteger())).toInt())
            index = (index_in_string(abc, symbol)).toBigInteger().add((rotorpos2))
            symbol = rotor_b.substring((index.remainder((abc.length).toBigInteger())).toInt(), ((index.remainder((abc.length).toBigInteger())).add((1).toBigInteger())).toInt())
            index = (index_in_string(abc, symbol)).toBigInteger().add((rotorpos3))
            symbol = rotor_c.substring((index.remainder((abc.length).toBigInteger())).toInt(), ((index.remainder((abc.length).toBigInteger())).add((1).toBigInteger())).toInt())
            symbol = reflector_map(symbol)
            index = (index_in_string(rotor_c, symbol)).toBigInteger().subtract((rotorpos3))
            if (index.compareTo((0).toBigInteger()) < 0) {
                index = index.add((abc.length).toBigInteger())
            }
            symbol = abc.substring((index).toInt(), (index.add((1).toBigInteger())).toInt())
            index = (index_in_string(rotor_b, symbol)).toBigInteger().subtract((rotorpos2))
            if (index.compareTo((0).toBigInteger()) < 0) {
                index = index.add((abc.length).toBigInteger())
            }
            symbol = abc.substring((index).toInt(), (index.add((1).toBigInteger())).toInt())
            index = (index_in_string(rotor_a, symbol)).toBigInteger().subtract((rotorpos1))
            if (index.compareTo((0).toBigInteger()) < 0) {
                index = index.add((abc.length).toBigInteger())
            }
            symbol = abc.substring((index).toInt(), (index.add((1).toBigInteger())).toInt())
            symbol = plugboard_map(plugboard, symbol)
            rotorpos1 = rotorpos1.add((1).toBigInteger())
            if (rotorpos1.compareTo((abc.length).toBigInteger()) >= 0) {
                rotorpos1 = (0.toBigInteger())
                rotorpos2 = rotorpos2.add((1).toBigInteger())
            }
            if (rotorpos2.compareTo((abc.length).toBigInteger()) >= 0) {
                rotorpos2 = (0.toBigInteger())
                rotorpos3 = rotorpos3.add((1).toBigInteger())
            }
            if (rotorpos3.compareTo((abc.length).toBigInteger()) >= 0) {
                rotorpos3 = (0.toBigInteger())
            }
        }
        result = result + symbol
        i = i + 1
    }
    return result
}

fun user_main(): Unit {
    var message: String = "This is my Python script that emulates the Enigma machine from WWII."
    var rotor_pos: MutableList<Int> = mutableListOf(1, 1, 1)
    var pb: String = "pictures"
    var rotor_sel: MutableList<String> = mutableListOf(rotor2, rotor4, rotor8)
    var en: String = enigma(message, rotor_pos, rotor_sel, pb)
    println("Encrypted message: " + en)
    println("Decrypted message: " + enigma(en, rotor_pos, rotor_sel, pb))
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
