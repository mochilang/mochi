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

var seed: Int = 1
var suits: String = "CDHS"
var nums: String = "A23456789TJQK"
fun rnd(): Int {
    seed = ((Math.floorMod((((seed * 214013) + 2531011).toLong()), 2147483648L)).toInt())
    return seed / 65536
}

fun deal(game: Int): MutableList<Int> {
    seed = game
    var deck: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < 52) {
        deck = run { val _tmp = deck.toMutableList(); _tmp.add(51 - i); _tmp }
        i = i + 1
    }
    i = 0
    while (i < 51) {
        var j: BigInteger = (51 - (Math.floorMod(rnd(), (52 - i)))).toBigInteger()
        var tmp: Int = deck[i]!!
        deck[i] = deck[(j).toInt()]!!
        deck[(j).toInt()] = tmp
        i = i + 1
    }
    return deck
}

fun show(cards: MutableList<Int>): Unit {
    var i: Int = 0
    while (i < cards.size) {
        var c: Int = cards[i]!!
        print((" " + nums.substring(c / 4, (c / 4) + 1)) + suits.substring(Math.floorMod(c, 4), (Math.floorMod(c, 4)) + 1))
        if (((Math.floorMod((i + 1), 8)) == 0) || ((i + 1) == cards.size)) {
            println("")
        }
        i = i + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("")
        println("Game #1")
        show(deal(1))
        println("")
        println("Game #617")
        show(deal(617))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
