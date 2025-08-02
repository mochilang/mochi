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

data class Info(var animal: String = "", var yinYang: String = "", var element: String = "", var stemBranch: String = "", var cycle: Int = 0)
var animal: MutableList<String> = mutableListOf("Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig")
var yinYang: MutableList<String> = mutableListOf("Yang", "Yin")
var element: MutableList<String> = mutableListOf("Wood", "Fire", "Earth", "Metal", "Water")
var stemChArr: MutableList<String> = mutableListOf("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸")
var branchChArr: MutableList<String> = mutableListOf("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥")
fun cz(yr: Int, animal: MutableList<String>, yinYang: MutableList<String>, element: MutableList<String>, sc: MutableList<String>, bc: MutableList<String>): Info {
    var y: BigInteger = (yr - 4).toBigInteger()
    var stem: BigInteger = y.remainder(10.toBigInteger())
    var branch: BigInteger = y.remainder(12.toBigInteger())
    var sb: String = sc[(stem).toInt()]!! + bc[(branch).toInt()]!!
    return Info(animal = (animal[(branch).toInt()]!!).toString(), yinYang = (yinYang[(stem.remainder(2.toBigInteger())).toInt()]!!).toString(), element = (element[(stem.divide(2.toBigInteger())).toInt()]!!).toString(), stemBranch = sb, cycle = ((y.remainder(60.toBigInteger())).add(1.toBigInteger())).toInt())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        for (yr in mutableListOf(1935, 1938, 1968, 1972, 1976)) {
            var r: Info = cz(yr, animal, yinYang, element, stemChArr, branchChArr)
            println((((((((((yr.toString() + ": ") + r.element) + " ") + r.animal) + ", ") + r.yinYang) + ", Cycle year ") + r.cycle.toString()) + " ") + r.stemBranch)
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
