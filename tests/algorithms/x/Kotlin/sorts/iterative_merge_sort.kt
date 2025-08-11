import java.math.BigInteger

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun merge(a: MutableList<Int>, low: Int, mid: Int, high: Int): MutableList<Int> {
    var left: MutableList<Int> = _sliceList(a, low, mid)
    var right: MutableList<Int> = _sliceList(a, mid, high + 1)
    var result: MutableList<Int> = mutableListOf<Int>()
    while ((left.size > 0) && (right.size > 0)) {
        if (left[0]!! <= right[0]!!) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(left[0]!!); _tmp }
            left = _sliceList(left, 1, left.size)
        } else {
            result = run { val _tmp = result.toMutableList(); _tmp.add(right[0]!!); _tmp }
            right = _sliceList(right, 1, right.size)
        }
    }
    var i: Int = (0).toInt()
    while (i < left.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(left[i]!!); _tmp }
        i = i + 1
    }
    i = 0
    while (i < right.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right[i]!!); _tmp }
        i = i + 1
    }
    i = 0
    while (i < result.size) {
        _listSet(a, low + i, result[i]!!)
        i = i + 1
    }
    return a
}

fun iter_merge_sort(items: MutableList<Int>): MutableList<Int> {
    var n: Int = (items.size).toInt()
    if (n <= 1) {
        return items
    }
    var arr: MutableList<Int> = _sliceList(items, 0, items.size)
    var p: Int = (2).toInt()
    while (p <= n) {
        var i: Int = (0).toInt()
        while (i < n) {
            var high: BigInteger = (((i + p) - 1).toBigInteger())
            if (high.compareTo((n).toBigInteger()) >= 0) {
                high = ((n - 1).toBigInteger())
            }
            var low: Int = (i).toInt()
            var mid = (((low).toBigInteger().add((high))).add((1).toBigInteger())).divide((2).toBigInteger())
            arr = merge(arr, low, (mid.toInt()), (high.toInt()))
            i = i + p
        }
        if ((p * 2) >= n) {
            var mid2: Int = (i - p).toInt()
            arr = merge(arr, 0, mid2, n - 1)
            break
        }
        p = p * 2
    }
    return arr
}

fun list_to_string(arr: MutableList<Int>): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < arr.size) {
        s = s + (arr[i]!!).toString()
        if (i < (arr.size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    return s + "]"
}

fun main() {
    println(list_to_string(iter_merge_sort(mutableListOf(5, 9, 8, 7, 1, 2, 7))))
    println(list_to_string(iter_merge_sort(mutableListOf(1))))
    println(list_to_string(iter_merge_sort(mutableListOf(2, 1))))
    println(list_to_string(iter_merge_sort(mutableListOf(4, 3, 2, 1))))
    println(list_to_string(iter_merge_sort(mutableListOf(5, 4, 3, 2, 1))))
    println(list_to_string(iter_merge_sort(mutableListOf(0 - 2, 0 - 9, 0 - 1, 0 - 4))))
    println(list_to_string(iter_merge_sort(mutableListOf<Int>())))
}
