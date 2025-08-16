fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

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

data class TextCounts(var single: MutableMap<String, Int> = mutableMapOf<String, Int>(), var double: MutableMap<String, Int> = mutableMapOf<String, Int>())
var text1: String = ("Behind Winston's back the voice " + "from the telescreen was still ") + "babbling and the overfulfilment"
fun log2(x: Double): Double {
    var k: Double = 0.0
    var v: Double = x
    while (v >= 2.0) {
        v = v / 2.0
        k = k + 1.0
    }
    while (v < 1.0) {
        v = v * 2.0
        k = k - 1.0
    }
    var z: Double = (v - 1.0) / (v + 1.0)
    var zpow: Double = z
    var sum: Double = z
    var i: Int = (3).toInt()
    while (i <= 9) {
        zpow = (zpow * z) * z
        sum = sum + (zpow / ((i.toDouble())))
        i = i + 2
    }
    var ln2: Double = 0.6931471805599453
    return k + ((2.0 * sum) / ln2)
}

fun analyze_text(text: String): TextCounts {
    var single: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var double: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var n: Int = (text.length).toInt()
    if (n == 0) {
        return TextCounts(single = single, double = double)
    }
    var last: String = text.substring(n - 1, n)
    if (last in single) {
        (single)[last] = (single)[last] as Int + 1
    } else {
        (single)[last] = 1
    }
    var first: String = text.substring(0, 1)
    var pair0: String = " " + first
    (double)[pair0] = 1
    var i: Int = (0).toInt()
    while (i < (n - 1)) {
        var ch: String = text.substring(i, i + 1)
        if (ch in single) {
            (single)[ch] = (single)[ch] as Int + 1
        } else {
            (single)[ch] = 1
        }
        var seq: String = text.substring(i, i + 2)
        if (seq in double) {
            (double)[seq] = (double)[seq] as Int + 1
        } else {
            (double)[seq] = 1
        }
        i = i + 1
    }
    return TextCounts(single = single, double = double)
}

fun round_to_int(x: Double): Int {
    if (x < 0.0) {
        return ((x - 0.5).toInt())
    }
    return ((x + 0.5).toInt())
}

fun calculate_entropy(text: String): Unit {
    var counts: TextCounts = analyze_text(text)
    var alphas: String = " abcdefghijklmnopqrstuvwxyz"
    var total1: Int = (0).toInt()
    for (ch in counts.single.keys) {
        total1 = total1 + (counts.single)[ch] as Int
    }
    var h1: Double = 0.0
    var i: Int = (0).toInt()
    while (i < alphas.length) {
        var ch: String = alphas.substring(i, i + 1)
        if (ch in counts.single) {
            var prob: Double = ((((counts.single)[ch] as Int).toDouble())) / ((total1.toDouble()))
            h1 = h1 + (prob * log2(prob))
        }
        i = i + 1
    }
    var first_entropy: Double = 0.0 - h1
    println(_numToStr(round_to_int(first_entropy)) + ".0")
    var total2: Int = (0).toInt()
    for (seq in counts.double.keys) {
        total2 = total2 + (counts.double)[seq] as Int
    }
    var h2: Double = 0.0
    var a0: Int = (0).toInt()
    while (a0 < alphas.length) {
        var ch0: String = alphas.substring(a0, a0 + 1)
        var a1: Int = (0).toInt()
        while (a1 < alphas.length) {
            var ch1: String = alphas.substring(a1, a1 + 1)
            var seq: String = ch0 + ch1
            if (seq in counts.double) {
                var prob: Double = ((((counts.double)[seq] as Int).toDouble())) / ((total2.toDouble()))
                h2 = h2 + (prob * log2(prob))
            }
            a1 = a1 + 1
        }
        a0 = a0 + 1
    }
    var second_entropy: Double = 0.0 - h2
    println(_numToStr(round_to_int(second_entropy)) + ".0")
    var diff: Double = second_entropy - first_entropy
    println(_numToStr(round_to_int(diff)) + ".0")
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        calculate_entropy(text1)
        var text3: String = ((((((((("Had repulsive dashwoods suspicion sincerity but advantage now him. " + "Remark easily garret nor nay.  Civil those mrs enjoy shy fat merry. ") + "You greatest jointure saw horrible. He private he on be imagine ") + "suppose. Fertile beloved evident through no service elderly is. Blind ") + "there if every no so at. Own neglected you preferred way sincerity ") + "delivered his attempted. To of message cottage windows do besides ") + "against uncivil.  Delightful unreserved impossible few estimating ") + "men favourable see entreaties. She propriety immediate was improving. ") + "He or entrance humoured likewise moderate. Much nor game son say ") + "feel. Fat make met can must form into gate. Me we offending prevailed ") + "discovery."
        calculate_entropy(text3)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
