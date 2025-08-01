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

var small: MutableList<String> = mutableListOf("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
var tens: MutableList<String> = mutableListOf("", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
var smallOrd: MutableList<String> = mutableListOf("zeroth", "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth", "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth", "eighteenth", "nineteenth")
var tensOrd: MutableList<String> = mutableListOf("", "", "twentieth", "thirtieth", "fortieth", "fiftieth", "sixtieth", "seventieth", "eightieth", "ninetieth")
var words: MutableList<String> = mutableListOf("Four", "is", "the", "number", "of", "letters", "in", "the", "first", "word", "of", "this", "sentence,")
var idx: Int = 0
fun say(n: Int): String {
    if (n < 20) {
        return small[n]!!
    }
    if (n < 100) {
        var res: String = tens[n / 10]!!
        var m: BigInteger = (Math.floorMod(n, 10)).toBigInteger()
        if (m.compareTo(0.toBigInteger()) != 0) {
            res = (res + "-") + small[(m).toInt()]!!
        }
        return res
    }
    if (n < 1000) {
        var res: String = say(n / 100) + " hundred"
        var m: BigInteger = (Math.floorMod(n, 100)).toBigInteger()
        if (m.compareTo(0.toBigInteger()) != 0) {
            res = (res + " ") + say(m.toInt())
        }
        return res
    }
    if (n < 1000000) {
        var res: String = say(n / 1000) + " thousand"
        var m: BigInteger = (Math.floorMod(n, 1000)).toBigInteger()
        if (m.compareTo(0.toBigInteger()) != 0) {
            res = (res + " ") + say(m.toInt())
        }
        return res
    }
    var res: String = say(n / 1000000) + " million"
    var m: BigInteger = (Math.floorMod(n, 1000000)).toBigInteger()
    if (m.compareTo(0.toBigInteger()) != 0) {
        res = (res + " ") + say(m.toInt())
    }
    return res
}

fun sayOrdinal(n: Int): String {
    if (n < 20) {
        return smallOrd[n]!!
    }
    if (n < 100) {
        if ((Math.floorMod(n, 10)) == 0) {
            return tensOrd[n / 10]!!
        }
        return (say(n - (Math.floorMod(n, 10))) + "-") + smallOrd[Math.floorMod(n, 10)]!!
    }
    if (n < 1000) {
        if ((Math.floorMod(n, 100)) == 0) {
            return say(n / 100) + " hundredth"
        }
        return (say(n / 100) + " hundred ") + sayOrdinal(Math.floorMod(n, 100))
    }
    if (n < 1000000) {
        if ((Math.floorMod(n, 1000)) == 0) {
            return say(n / 1000) + " thousandth"
        }
        return (say(n / 1000) + " thousand ") + sayOrdinal(Math.floorMod(n, 1000))
    }
    if ((Math.floorMod(n, 1000000)) == 0) {
        return say(n / 1000000) + " millionth"
    }
    return (say(n / 1000000) + " million ") + sayOrdinal(Math.floorMod(n, 1000000))
}

fun split(s: String, sep: String): MutableList<String> {
    var parts: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = 0
    while (i < s.length) {
        if ((((sep.length > 0) && ((i + sep.length) <= s.length) as Boolean)) && (s.substring(i, i + sep.length) == sep)) {
            parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
            cur = ""
            i = i + sep.length
        } else {
            cur = cur + (s.substring(i, i + 1)).toString()
            i = i + 1
        }
    }
    parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp } as MutableList<String>
    return parts
}

fun countLetters(s: String): Int {
    var cnt: Int = 0
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        if ((((ch >= "A") && (ch <= "Z") as Boolean)) || (((ch >= "a") && (ch <= "z") as Boolean))) {
            cnt = cnt + 1
        }
        i = i + 1
    }
    return cnt
}

fun wordLen(w: Int): MutableList<Any?> {
    while (words.size < w) {
        idx = idx + 1
        var n: Int = countLetters(words[idx]!!)
        var parts: MutableList<String> = split(say(n), " ")
        var j: Int = 0
        while (j < parts.size) {
            words = run { val _tmp = words.toMutableList(); _tmp.add(parts[j]!!); _tmp } as MutableList<String>
            j = j + 1
        }
        words = run { val _tmp = words.toMutableList(); _tmp.add("in"); _tmp } as MutableList<String>
        words = run { val _tmp = words.toMutableList(); _tmp.add("the"); _tmp } as MutableList<String>
        parts = split(sayOrdinal(idx + 1) + ",", " ")
        j = 0
        while (j < parts.size) {
            words = run { val _tmp = words.toMutableList(); _tmp.add(parts[j]!!); _tmp } as MutableList<String>
            j = j + 1
        }
    }
    var word: String = words[w - 1]!!
    return mutableListOf<Any?>(word as Any?, (countLetters(word)) as Any?)
}

fun totalLength(): Int {
    var tot: Int = 0
    var i: Int = 0
    while (i < words.size) {
        tot = tot + (words[i]!!).length
        if (i < (words.size - 1)) {
            tot = tot + 1
        }
        i = i + 1
    }
    return tot
}

fun pad(n: Int, width: Int): String {
    var s: String = n.toString()
    while (s.length < width) {
        s = " " + s
    }
    return s
}

fun user_main(): Unit {
    println("The lengths of the first 201 words are:")
    var line: String = ""
    var i: Int = 1
    while (i <= 201) {
        if ((Math.floorMod(i, 25)) == 1) {
            if (i != 1) {
                println(line)
            }
            line = pad(i, 3) + ":"
        }
        var r: MutableList<Any?> = wordLen(i)
        var n: Any? = r[1] as Any?
        line = (line + " ") + pad(n as Int, 2)
        i = i + 1
    }
    println(line)
    println("Length of sentence so far: " + totalLength().toString())
    for (n in mutableListOf(1000, 10000, 100000, 1000000, 10000000)) {
        var r: MutableList<Any?> = wordLen(n)
        var w: Any? = r[0] as Any?
        var l: Any? = r[1] as Any?
        println((((((("Word " + pad(n, 8)) + " is \"") + (w).toString()) + "\", with ") + l.toString()) + " letters.  Length of sentence so far: ") + totalLength().toString())
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
