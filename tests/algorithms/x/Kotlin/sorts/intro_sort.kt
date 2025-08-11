import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var example1: MutableList<Int> = mutableListOf(4, 2, 6, 8, 1, 7, 8, 22, 14, 56, 27, 79, 23, 45, 14, 12)
fun insertion_sort(a: MutableList<Int>, start: Int, end_: Int): MutableList<Int> {
    var arr: MutableList<Int> = a
    var i: Int = (start).toInt()
    while (i < end_) {
        var key: Int = (arr[i]!!).toInt()
        var j: Int = (i).toInt()
        while ((j > start) && (arr[j - 1]!! > key)) {
            _listSet(arr, j, arr[j - 1]!!)
            j = j - 1
        }
        _listSet(arr, j, key)
        i = i + 1
    }
    return arr
}

fun heapify(a: MutableList<Int>, index: Int, heap_size: Int): MutableList<Int> {
    var arr: MutableList<Int> = a
    var largest: Int = (index).toInt()
    var left: Int = ((2 * index) + 1).toInt()
    var right: Int = ((2 * index) + 2).toInt()
    if ((left < heap_size) && (arr[left]!! > arr[largest]!!)) {
        largest = left
    }
    if ((right < heap_size) && (arr[right]!! > arr[largest]!!)) {
        largest = right
    }
    if (largest != index) {
        var temp: Int = (arr[index]!!).toInt()
        _listSet(arr, index, arr[largest]!!)
        _listSet(arr, largest, temp)
        arr = heapify(arr, largest, heap_size)
    }
    return arr
}

fun heap_sort(a: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = a
    var n: Int = (arr.size).toInt()
    if (n <= 1) {
        return arr
    }
    var i: Int = (n / 2).toInt()
    while (true) {
        arr = heapify(arr, i, n)
        if (i == 0) {
            break
        }
        i = i - 1
    }
    i = n - 1
    while (i > 0) {
        var temp: Int = (arr[0]!!).toInt()
        _listSet(arr, 0, arr[i]!!)
        _listSet(arr, i, temp)
        arr = heapify(arr, 0, i)
        i = i - 1
    }
    return arr
}

fun median_of_3(arr: MutableList<Int>, first: Int, middle: Int, last: Int): Int {
    var a: Int = (arr[first]!!).toInt()
    var b: Int = (arr[middle]!!).toInt()
    var c: Int = (arr[last]!!).toInt()
    if ((((a > b) && (a < c) as Boolean)) || (((a < b) && (a > c) as Boolean))) {
        return a
    } else {
        if ((((b > a) && (b < c) as Boolean)) || (((b < a) && (b > c) as Boolean))) {
            return b
        } else {
            return c
        }
    }
}

fun partition(arr: MutableList<Int>, low: Int, high: Int, pivot: Int): Int {
    var i: Int = (low).toInt()
    var j: Int = (high).toInt()
    while (true) {
        while (arr[i]!! < pivot) {
            i = i + 1
        }
        j = j - 1
        while (pivot < arr[j]!!) {
            j = j - 1
        }
        if (i >= j) {
            return i
        }
        var temp: Int = (arr[i]!!).toInt()
        _listSet(arr, i, arr[j]!!)
        _listSet(arr, j, temp)
        i = i + 1
    }
}

fun int_log2(n: Int): Int {
    var v: Int = (n).toInt()
    var r: Int = (0).toInt()
    while (v > 1) {
        v = v / 2
        r = r + 1
    }
    return r
}

fun intro_sort(arr: MutableList<Int>, start: Int, end_: Int, size_threshold: Int, max_depth: Int): MutableList<Int> {
    var array: MutableList<Int> = arr
    var s: Int = (start).toInt()
    var e: Int = (end_).toInt()
    var depth: Int = (max_depth).toInt()
    while ((e - s) > size_threshold) {
        if (depth == 0) {
            return heap_sort(array)
        }
        depth = depth - 1
        var pivot: Int = (median_of_3(array, s, (s + ((e - s) / 2)) + 1, e - 1)).toInt()
        var p: Int = (partition(array, s, e, pivot)).toInt()
        array = intro_sort(array, p, e, size_threshold, depth)
        e = p
    }
    var res: MutableList<Int> = insertion_sort(array, s, e)
    var _u1: Int = (res.size).toInt()
    return res
}

fun intro_sort_main(arr: MutableList<Int>): Unit {
    if (arr.size == 0) {
        println(arr)
        return
    }
    var max_depth: Int = (2 * int_log2(arr.size)).toInt()
    var sorted: MutableList<Int> = intro_sort(arr, 0, arr.size, 16, max_depth)
    println(sorted)
}

fun main() {
    intro_sort_main(example1)
    var example2: MutableList<Int> = mutableListOf(21, 15, 11, 45, 0 - 2, 0 - 11, 46)
    intro_sort_main(example2)
}
