import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun make_list(n: Int, value: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < n) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(value); _tmp }
        i = i + 1
    }
    return result
}

fun min_value(arr: MutableList<Int>): Int {
    var m: Int = (arr[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < arr.size) {
        if (arr[i]!! < m) {
            m = arr[i]!!
        }
        i = i + 1
    }
    return m
}

fun max_value(arr: MutableList<Int>): Int {
    var m: Int = (arr[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < arr.size) {
        if (arr[i]!! > m) {
            m = arr[i]!!
        }
        i = i + 1
    }
    return m
}

fun pigeon_sort(array: MutableList<Int>): MutableList<Int> {
    if (array.size == 0) {
        return array
    }
    var mn: Int = (min_value(array)).toInt()
    var mx: Int = (max_value(array)).toInt()
    var holes_range: Int = ((mx - mn) + 1).toInt()
    var holes: MutableList<Int> = make_list(holes_range, 0)
    var holes_repeat: MutableList<Int> = make_list(holes_range, 0)
    var i: Int = (0).toInt()
    while (i < array.size) {
        var index: Int = (array[i]!! - mn).toInt()
        _listSet(holes, index, array[i]!!)
        _listSet(holes_repeat, index, holes_repeat[index]!! + 1)
        i = i + 1
    }
    var array_index: Int = (0).toInt()
    var h: Int = (0).toInt()
    while (h < holes_range) {
        while (holes_repeat[h]!! > 0) {
            _listSet(array, array_index, holes[h]!!)
            array_index = array_index + 1
            _listSet(holes_repeat, h, holes_repeat[h]!! - 1)
        }
        h = h + 1
    }
    return array
}

fun main() {
    println(pigeon_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
    println(pigeon_sort(mutableListOf<Int>()).toString())
    println(pigeon_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45)).toString())
}
