fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun flip(arr: MutableList<Int>, k: Int): MutableList<Int> {
    var start: Int = (0).toInt()
    var end: Int = (k).toInt()
    while (start < end) {
        var temp: Int = (arr[start]!!).toInt()
        _listSet(arr, start, arr[end]!!)
        _listSet(arr, end, temp)
        start = start + 1
        end = end - 1
    }
    return arr
}

fun find_max_index(arr: MutableList<Int>, n: Int): Int {
    var mi: Int = (0).toInt()
    var i: Int = (1).toInt()
    while (i < n) {
        if (arr[i]!! > arr[mi]!!) {
            mi = i
        }
        i = i + 1
    }
    return mi
}

fun pancake_sort(arr: MutableList<Int>): MutableList<Int> {
    var arr: MutableList<Int> = arr
    var cur: Int = (arr.size).toInt()
    while (cur > 1) {
        var mi: Int = (find_max_index(arr, cur)).toInt()
        arr = flip(arr, mi)
        arr = flip(arr, cur - 1)
        cur = cur - 1
    }
    return arr
}

fun user_main(): Unit {
    var data: MutableList<Int> = mutableListOf(3, 6, 1, 10, 2)
    var sorted: MutableList<Int> = pancake_sort(data)
    println(sorted.toString())
}

fun main() {
    user_main()
}
