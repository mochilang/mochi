fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun stooge(arr: MutableList<Int>, i: Int, h: Int): Unit {
    if (i >= h) {
        return
    }
    if (arr[i]!! > arr[h]!!) {
        var tmp: Int = (arr[i]!!).toInt()
        _listSet(arr, i, arr[h]!!)
        _listSet(arr, h, tmp)
    }
    if (((h - i) + 1) > 2) {
        var t: Int = (((((h - i) + 1) / 3).toInt())).toInt()
        stooge(arr, i, h - t)
        stooge(arr, i + t, h)
        stooge(arr, i, h - t)
    }
}

fun stooge_sort(arr: MutableList<Int>): MutableList<Int> {
    stooge(arr, 0, arr.size - 1)
    return arr
}

fun main() {
    println(stooge_sort(mutableListOf(18, 0, 0 - 7, 0 - 1, 2, 2)).toString())
}
