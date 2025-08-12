import java.math.BigInteger

fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
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

data class NumberContainer(var numbermap: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>(), var indexmap: MutableMap<Int, Int> = mutableMapOf<Int, Int>())
var nm: MutableMap<Int, MutableList<Int>> = mutableMapOf<Int, MutableList<Int>>()
var im: MutableMap<Int, Int> = mutableMapOf<Int, Int>()
var cont: NumberContainer = NumberContainer(numbermap = nm, indexmap = im)
fun remove_at(xs: MutableList<Int>, idx: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i != idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun insert_at(xs: MutableList<Int>, idx: Int, _val: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i == idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(_val); _tmp }
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    if (idx == xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(_val); _tmp }
    }
    return res
}

fun binary_search_delete(array: MutableList<Int>, item: Int): MutableList<Int> {
    var low: Int = (0).toInt()
    var high: BigInteger = ((array.size - 1).toBigInteger())
    var arr: MutableList<Int> = array
    while ((low).toBigInteger().compareTo((high)) <= 0) {
        var mid = ((low).toBigInteger().add((high))).divide((2).toBigInteger())
        if (arr[(mid).toInt()]!! == item) {
            arr = remove_at(arr, (mid.toInt()))
            return arr
        } else {
            if (arr[(mid).toInt()]!! < item) {
                low = ((mid.add((1).toBigInteger())).toInt())
            } else {
                high = mid.subtract((1).toBigInteger())
            }
        }
    }
    println("ValueError: Either the item is not in the array or the array was unsorted")
    return arr
}

fun binary_search_insert(array: MutableList<Int>, index: Int): MutableList<Int> {
    var low: Int = (0).toInt()
    var high: BigInteger = ((array.size - 1).toBigInteger())
    var arr: MutableList<Int> = array
    while ((low).toBigInteger().compareTo((high)) <= 0) {
        var mid = ((low).toBigInteger().add((high))).divide((2).toBigInteger())
        if (arr[(mid).toInt()]!! == index) {
            arr = insert_at(arr, ((mid.add((1).toBigInteger())).toInt()), index)
            return arr
        } else {
            if (arr[(mid).toInt()]!! < index) {
                low = ((mid.add((1).toBigInteger())).toInt())
            } else {
                high = mid.subtract((1).toBigInteger())
            }
        }
    }
    arr = insert_at(arr, low, index)
    return arr
}

fun change(cont: NumberContainer, idx: Int, num: Int): NumberContainer {
    var numbermap: MutableMap<Int, MutableList<Int>> = cont.numbermap
    var indexmap: MutableMap<Int, Int> = cont.indexmap
    if (idx in indexmap) {
        var old: Int = ((indexmap)[idx] as Int).toInt()
        var indexes: MutableList<Int> = (numbermap)[old] as MutableList<Int>
        if (indexes.size == 1) {
            (numbermap)[old] = mutableListOf<Int>()
        } else {
            (numbermap)[old] = binary_search_delete(indexes, idx)
        }
    }
    (indexmap)[idx] = num
    if (num in numbermap) {
        (numbermap)[num] = binary_search_insert((numbermap)[num] as MutableList<Int>, idx)
    } else {
        (numbermap)[num] = mutableListOf(idx)
    }
    return NumberContainer(numbermap = numbermap, indexmap = indexmap)
}

fun find(cont: NumberContainer, num: Int): Int {
    var numbermap: MutableMap<Int, MutableList<Int>> = cont.numbermap
    if (num in numbermap) {
        var arr: MutableList<Int> = (numbermap)[num] as MutableList<Int>
        if (_len(arr) > 0) {
            return (((arr as MutableList<Any?>)[0]) as Int)
        }
    }
    return 0 - 1
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(find(cont, 10))
        cont = change(cont, 0, 10)
        println(find(cont, 10))
        cont = change(cont, 0, 20)
        println(find(cont, 10))
        println(find(cont, 20))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
