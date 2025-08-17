fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

var m2: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 5.0), mutableListOf(2.0, 0.0))
fun inverse_of_matrix(matrix: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    if ((((matrix.size == 2) && ((matrix[0]!!).size == 2) as Boolean)) && ((matrix[1]!!).size == 2)) {
        var det: Double = (((matrix[0]!!) as MutableList<Double>)[0]!! * ((matrix[1]!!) as MutableList<Double>)[1]!!) - (((matrix[1]!!) as MutableList<Double>)[0]!! * ((matrix[0]!!) as MutableList<Double>)[1]!!)
        if (det == 0.0) {
            println("This matrix has no inverse.")
            return mutableListOf<MutableList<Double>>()
        }
        return mutableListOf(mutableListOf(((matrix[1]!!) as MutableList<Double>)[1]!! / det, (0.0 - ((matrix[0]!!) as MutableList<Double>)[1]!!) / det), mutableListOf((0.0 - ((matrix[1]!!) as MutableList<Double>)[0]!!) / det, ((matrix[0]!!) as MutableList<Double>)[0]!! / det))
    } else {
        if ((((((matrix.size == 3) && ((matrix[0]!!).size == 3) as Boolean)) && ((matrix[1]!!).size == 3) as Boolean)) && ((matrix[2]!!).size == 3)) {
            var det: Double = ((((((matrix[0]!!) as MutableList<Double>)[0]!! * ((matrix[1]!!) as MutableList<Double>)[1]!!) * ((matrix[2]!!) as MutableList<Double>)[2]!!) + ((((matrix[0]!!) as MutableList<Double>)[1]!! * ((matrix[1]!!) as MutableList<Double>)[2]!!) * ((matrix[2]!!) as MutableList<Double>)[0]!!)) + ((((matrix[0]!!) as MutableList<Double>)[2]!! * ((matrix[1]!!) as MutableList<Double>)[0]!!) * ((matrix[2]!!) as MutableList<Double>)[1]!!)) - ((((((matrix[0]!!) as MutableList<Double>)[2]!! * ((matrix[1]!!) as MutableList<Double>)[1]!!) * ((matrix[2]!!) as MutableList<Double>)[0]!!) + ((((matrix[0]!!) as MutableList<Double>)[1]!! * ((matrix[1]!!) as MutableList<Double>)[0]!!) * ((matrix[2]!!) as MutableList<Double>)[2]!!)) + ((((matrix[0]!!) as MutableList<Double>)[0]!! * ((matrix[1]!!) as MutableList<Double>)[2]!!) * ((matrix[2]!!) as MutableList<Double>)[1]!!))
            if (det == 0.0) {
                println("This matrix has no inverse.")
                return mutableListOf<MutableList<Double>>()
            }
            var cof: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 0.0))
            _listSet(cof[0]!!, 0, (((matrix[1]!!) as MutableList<Double>)[1]!! * ((matrix[2]!!) as MutableList<Double>)[2]!!) - (((matrix[1]!!) as MutableList<Double>)[2]!! * ((matrix[2]!!) as MutableList<Double>)[1]!!))
            _listSet(cof[0]!!, 1, 0.0 - ((((matrix[1]!!) as MutableList<Double>)[0]!! * ((matrix[2]!!) as MutableList<Double>)[2]!!) - (((matrix[1]!!) as MutableList<Double>)[2]!! * ((matrix[2]!!) as MutableList<Double>)[0]!!)))
            _listSet(cof[0]!!, 2, (((matrix[1]!!) as MutableList<Double>)[0]!! * ((matrix[2]!!) as MutableList<Double>)[1]!!) - (((matrix[1]!!) as MutableList<Double>)[1]!! * ((matrix[2]!!) as MutableList<Double>)[0]!!))
            _listSet(cof[1]!!, 0, 0.0 - ((((matrix[0]!!) as MutableList<Double>)[1]!! * ((matrix[2]!!) as MutableList<Double>)[2]!!) - (((matrix[0]!!) as MutableList<Double>)[2]!! * ((matrix[2]!!) as MutableList<Double>)[1]!!)))
            _listSet(cof[1]!!, 1, (((matrix[0]!!) as MutableList<Double>)[0]!! * ((matrix[2]!!) as MutableList<Double>)[2]!!) - (((matrix[0]!!) as MutableList<Double>)[2]!! * ((matrix[2]!!) as MutableList<Double>)[0]!!))
            _listSet(cof[1]!!, 2, 0.0 - ((((matrix[0]!!) as MutableList<Double>)[0]!! * ((matrix[2]!!) as MutableList<Double>)[1]!!) - (((matrix[0]!!) as MutableList<Double>)[1]!! * ((matrix[2]!!) as MutableList<Double>)[0]!!)))
            _listSet(cof[2]!!, 0, (((matrix[0]!!) as MutableList<Double>)[1]!! * ((matrix[1]!!) as MutableList<Double>)[2]!!) - (((matrix[0]!!) as MutableList<Double>)[2]!! * ((matrix[1]!!) as MutableList<Double>)[1]!!))
            _listSet(cof[2]!!, 1, 0.0 - ((((matrix[0]!!) as MutableList<Double>)[0]!! * ((matrix[1]!!) as MutableList<Double>)[2]!!) - (((matrix[0]!!) as MutableList<Double>)[2]!! * ((matrix[1]!!) as MutableList<Double>)[0]!!)))
            _listSet(cof[2]!!, 2, (((matrix[0]!!) as MutableList<Double>)[0]!! * ((matrix[1]!!) as MutableList<Double>)[1]!!) - (((matrix[0]!!) as MutableList<Double>)[1]!! * ((matrix[1]!!) as MutableList<Double>)[0]!!))
            var inv: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 0.0), mutableListOf(0.0, 0.0, 0.0))
            var i: Int = (0).toInt()
            while (i < 3) {
                var j: Int = (0).toInt()
                while (j < 3) {
                    _listSet(inv[i]!!, j, ((cof[j]!!) as MutableList<Double>)[i]!! / det)
                    j = j + 1
                }
                i = i + 1
            }
            return inv
        }
    }
    println("Please provide a matrix of size 2x2 or 3x3.")
    return mutableListOf<MutableList<Double>>()
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(inverse_of_matrix(m2))
        var m3: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(2.0, 5.0, 7.0), mutableListOf(2.0, 0.0, 1.0), mutableListOf(1.0, 2.0, 3.0))
        println(inverse_of_matrix(m3))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
