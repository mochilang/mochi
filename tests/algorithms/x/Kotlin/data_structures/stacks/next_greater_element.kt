import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

var arr: MutableList<Double> = mutableListOf(0.0 - 10.0, 0.0 - 5.0, 0.0, 5.0, 5.1, 11.0, 13.0, 21.0, 3.0, 4.0, 0.0 - 21.0, 0.0 - 10.0, 0.0 - 5.0, 0.0 - 1.0, 0.0)
var expected: MutableList<Double> = mutableListOf(0.0 - 5.0, 0.0, 5.0, 5.1, 11.0, 13.0, 21.0, 0.0 - 1.0, 4.0, 0.0 - 1.0, 0.0 - 10.0, 0.0 - 5.0, 0.0 - 1.0, 0.0, 0.0 - 1.0)
fun next_greatest_element_slow(xs: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        var next: Double = 0.0 - 1.0
        var j: Int = (i + 1).toInt()
        while (j < xs.size) {
            if (xs[i]!! < xs[j]!!) {
                next = xs[j]!!
                break
            }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(next); _tmp }
        i = i + 1
    }
    return res
}

fun next_greatest_element_fast(xs: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        var next: Double = 0.0 - 1.0
        var j: Int = (i + 1).toInt()
        while (j < xs.size) {
            var inner: Double = xs[j]!!
            if (xs[i]!! < inner) {
                next = inner
                break
            }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(next); _tmp }
        i = i + 1
    }
    return res
}

fun set_at_float(xs: MutableList<Double>, idx: Int, value: Double): MutableList<Double> {
    var i: Int = (0).toInt()
    var res: MutableList<Double> = mutableListOf<Double>()
    while (i < xs.size) {
        if (i == idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun next_greatest_element(xs: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var k: Int = (0).toInt()
    while (k < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(0.0 - 1.0); _tmp }
        k = k + 1
    }
    var stack: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        while ((stack.size > 0) && (xs[i]!! > xs[stack[stack.size - 1]!!]!!)) {
            var idx: Int = (stack[stack.size - 1]!!).toInt()
            stack = _sliceList(stack, 0, stack.size - 1)
            res = set_at_float(res, idx, xs[i]!!)
        }
        stack = run { val _tmp = stack.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    return res
}

fun main() {
    println(next_greatest_element_slow(arr).toString())
    println(next_greatest_element_fast(arr).toString())
    println(next_greatest_element(arr).toString())
}
