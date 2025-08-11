fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun subarray(xs: MutableList<Int>, start: Int, end: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var k: Int = (start).toInt()
    while (k < end) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(xs[k]!!); _tmp }
        k = k + 1
    }
    return result
}

fun merge(arr: MutableList<Int>): MutableList<Int> {
    if (arr.size > 1) {
        var middle_length: Int = (arr.size / 2).toInt()
        var left_array: MutableList<Int> = subarray(arr, 0, middle_length)
        var right_array: MutableList<Int> = subarray(arr, middle_length, arr.size)
        var left_size: Int = (left_array.size).toInt()
        var right_size: Int = (right_array.size).toInt()
        merge(left_array)
        merge(right_array)
        var left_index: Int = (0).toInt()
        var right_index: Int = (0).toInt()
        var index: Int = (0).toInt()
        while ((left_index < left_size) && (right_index < right_size)) {
            if (left_array[left_index]!! < right_array[right_index]!!) {
                _listSet(arr, index, left_array[left_index]!!)
                left_index = left_index + 1
            } else {
                _listSet(arr, index, right_array[right_index]!!)
                right_index = right_index + 1
            }
            index = index + 1
        }
        while (left_index < left_size) {
            _listSet(arr, index, left_array[left_index]!!)
            left_index = left_index + 1
            index = index + 1
        }
        while (right_index < right_size) {
            _listSet(arr, index, right_array[right_index]!!)
            right_index = right_index + 1
            index = index + 1
        }
    }
    return arr
}

fun main() {
    println(merge(mutableListOf(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)).toString())
    println(merge(mutableListOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)).toString())
    println(merge(mutableListOf(10, 22, 1, 2, 3, 9, 15, 23)).toString())
    println(merge(mutableListOf(100)).toString())
    println(merge(mutableListOf<Int>()).toString())
}
