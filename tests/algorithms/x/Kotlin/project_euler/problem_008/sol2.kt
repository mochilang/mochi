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

var N: String = (((((((((((((((((("73167176531330624919225119674426574742355349194934" + "96983520312774506326239578318016984801869478851843") + "85861560789112949495459501737958331952853208805511") + "12540698747158523863050715693290963295227443043557") + "66896648950445244523161731856403098711121722383113") + "62229893423380308135336276614282806444486645238749") + "30358907296290491560440772390713810515859307960866") + "70172427121883998797908792274921901699720888093776") + "65727333001053367881220235421809751254540594752243") + "52584907711670556013604839586446706324415722155397") + "53697817977846174064955149290862569321978468622482") + "83972241375657056057490261407972968652414535100474") + "82166370484403199890008895243450658541227588666881") + "16427171479924442928230863465674813919123162824586") + "17866458359124566529476545682848912883142607690042") + "24219022671055626321111109370544217506941658960408") + "07198403850962455444362981230987879927244284909188") + "84580156166097919133875499200524063689912560717606") + "05886116467109405077541002256983155200055935729725") + "71636269561882670428252483600823257530420752963450"
fun solution(n: String): Int {
    var max_product: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i <= (n.length - 13)) {
        var product: Int = (1).toInt()
        var j: Int = (0).toInt()
        while (j < 13) {
            product = product * n[i + j].toString().toInt()
            j = j + 1
        }
        if (product > max_product) {
            max_product = product
        }
        i = i + 1
    }
    return max_product
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(solution("13978431290823798458352374"))
        println(solution("13978431295823798458352374"))
        println(solution("1397843129582379841238352374"))
        println(solution(N))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
