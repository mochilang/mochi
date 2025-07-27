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

fun qsel(a: MutableList<Double>, k: Int): Double {
    var k: Int = k
    var arr: MutableList<Double> = a
    while (arr.size > 1) {
        var px: BigInteger = (Math.floorMod(_now(), arr.size)).toBigInteger()
        var pv: Double = arr[(px).toInt()]
        val last: Int = arr.size - 1
        val tmp: Double = arr[(px).toInt()]
        arr[(px).toInt()] = arr[last]
        arr[last] = tmp
        px = 0.toBigInteger()
        var i: Int = 0
        while (i < last) {
            val v: Double = arr[i]
            if (v < pv) {
                val tmp2: Double = arr[(px).toInt()]
                arr[(px).toInt()] = arr[i]
                arr[i] = tmp2
                px = px.add(1.toBigInteger())
            }
            i = i + 1
        }
        if (px.compareTo(k.toBigInteger()) == 0) {
            return pv
        }
        if (k.toBigInteger().compareTo(px) < 0) {
            arr = arr.subList(0, (px).toInt())
        } else {
            val tmp2: Double = arr[(px).toInt()]
            arr[(px).toInt()] = pv
            arr[last] = tmp2
            arr = arr.subList((px.add(1.toBigInteger())).toInt(), arr.size)
            k = (k.toBigInteger().subtract((px.add(1.toBigInteger())))).toInt()
        }
    }
    return arr[0]
}

fun median(list: MutableList<Double>): Double {
    var arr: MutableList<Double> = list
    val half: Int = (arr.size / 2).toInt()
    val med: Double = qsel(arr, half)
    if ((Math.floorMod(arr.size, 2)) == 0) {
        return (med + qsel(arr, half - 1)) / 2.0
    }
    return med
}

fun main() {
    println(median(mutableListOf(3.0, 1.0, 4.0, 1.0)).toString())
    println(median(mutableListOf(3.0, 1.0, 4.0, 1.0, 5.0)).toString())
}
