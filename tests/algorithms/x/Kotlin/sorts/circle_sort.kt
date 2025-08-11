import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun circle_sort_util(collection: MutableList<Int>, low: Int, high: Int): Boolean {
    var swapped: Boolean = false
    if (low == high) {
        return swapped
    }
    var left: Int = (low).toInt()
    var right: Int = (high).toInt()
    while (left < right) {
        if (collection[left]!! > collection[right]!!) {
            var tmp: Int = (collection[left]!!).toInt()
            _listSet(collection, left, collection[right]!!)
            _listSet(collection, right, tmp)
            swapped = true
        }
        left = left + 1
        right = right - 1
    }
    if ((left == right) && (collection[left]!! > collection[right + 1]!!)) {
        var tmp2: Int = (collection[left]!!).toInt()
        _listSet(collection, left, collection[right + 1]!!)
        _listSet(collection, right + 1, tmp2)
        swapped = true
    }
    var mid: Int = (low + ((high - low) / 2)).toInt()
    var left_swap: Boolean = circle_sort_util(collection, low, mid)
    var right_swap: Boolean = circle_sort_util(collection, mid + 1, high)
    if (((swapped || left_swap as Boolean)) || right_swap) {
        return true
    } else {
        return false
    }
}

fun circle_sort(collection: MutableList<Int>): MutableList<Int> {
    if (collection.size < 2) {
        return collection
    }
    var is_not_sorted: Boolean = true
    while ((is_not_sorted as Boolean)) {
        is_not_sorted = circle_sort_util(collection, 0, collection.size - 1)
    }
    return collection
}

fun main() {
    println(circle_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
    println(circle_sort(mutableListOf<Int>()).toString())
    println(circle_sort(mutableListOf(0 - 2, 5, 0, 0 - 45)).toString())
}
