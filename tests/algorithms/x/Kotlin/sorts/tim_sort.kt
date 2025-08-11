import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var sample: MutableList<Int> = mutableListOf(5, 9, 10, 3, 0 - 4, 5, 178, 92, 46, 0 - 18, 0, 7)
var sorted_sample: MutableList<Int> = tim_sort(sample)
fun copy_list(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (0).toInt()
    while (k < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[k]!!); _tmp }
        k = k + 1
    }
    return res
}

fun insertion_sort(xs: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = copy_list(xs)
    var idx: Int = (1).toInt()
    while (idx < arr.size) {
        var value: Int = (arr[idx]!!).toInt()
        var jdx: BigInteger = ((idx - 1).toBigInteger())
        while ((jdx.compareTo((0).toBigInteger()) >= 0) && (arr[(jdx).toInt()]!! > value)) {
            _listSet(arr, (jdx.add((1).toBigInteger())).toInt(), arr[(jdx).toInt()]!!)
            jdx = jdx.subtract((1).toBigInteger())
        }
        _listSet(arr, (jdx.add((1).toBigInteger())).toInt(), value)
        idx = idx + 1
    }
    return arr
}

fun merge(left: MutableList<Int>, right: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var j: Int = (0).toInt()
    while ((i < left.size) && (j < right.size)) {
        if (left[i]!! < right[j]!!) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(left[i]!!); _tmp }
            i = i + 1
        } else {
            result = run { val _tmp = result.toMutableList(); _tmp.add(right[j]!!); _tmp }
            j = j + 1
        }
    }
    while (i < left.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(left[i]!!); _tmp }
        i = i + 1
    }
    while (j < right.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right[j]!!); _tmp }
        j = j + 1
    }
    return result
}

fun tim_sort(xs: MutableList<Int>): MutableList<Int> {
    var n: Int = (xs.size).toInt()
    var runs: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var sorted_runs: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var current: MutableList<Int> = mutableListOf<Int>()
    current = run { val _tmp = current.toMutableList(); _tmp.add(xs[0]!!); _tmp }
    var i: Int = (1).toInt()
    while (i < n) {
        if (xs[i]!! < xs[i - 1]!!) {
            runs = run { val _tmp = runs.toMutableList(); _tmp.add(copy_list(current)); _tmp }
            current = mutableListOf<Int>()
            current = run { val _tmp = current.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        } else {
            current = run { val _tmp = current.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    runs = run { val _tmp = runs.toMutableList(); _tmp.add(copy_list(current)); _tmp }
    var r: Int = (0).toInt()
    while (r < runs.size) {
        sorted_runs = run { val _tmp = sorted_runs.toMutableList(); _tmp.add(insertion_sort(runs[r]!!)); _tmp }
        r = r + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    r = 0
    while (r < sorted_runs.size) {
        result = merge(result, sorted_runs[r]!!)
        r = r + 1
    }
    return result
}

fun list_to_string(xs: MutableList<Int>): String {
    var s: String = "["
    var k: Int = (0).toInt()
    while (k < xs.size) {
        s = s + (xs[k]!!).toString()
        if (k < (xs.size - 1)) {
            s = s + ", "
        }
        k = k + 1
    }
    return s + "]"
}

fun main() {
    println(list_to_string(sorted_sample))
    var sample2: MutableList<Int> = mutableListOf(3, 2, 1)
    var sorted_sample2: MutableList<Int> = tim_sort(sample2)
    println(list_to_string(sorted_sample2))
}
