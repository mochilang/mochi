import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

fun is_sorted(xs: MutableList<Int>): Boolean {
    var i: Int = (1).toInt()
    while (i < xs.size) {
        if (xs[i - 1]!! > xs[i]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun exponential_search(arr: MutableList<Int>, item: Int): Int {
    if (!is_sorted(arr)) {
        panic("sorted_collection must be sorted in ascending order")
    }
    if (arr.size == 0) {
        return 0 - 1
    }
    if (arr[0]!! == item) {
        return 0
    }
    var bound: Int = (1).toInt()
    while ((bound < arr.size) && (arr[bound]!! < item)) {
        bound = bound * 2
    }
    var left: Int = (bound / 2).toInt()
    var right: Int = (bound).toInt()
    if (right >= arr.size) {
        right = arr.size - 1
    }
    while (left <= right) {
        var mid: Int = (left + ((right - left) / 2)).toInt()
        if (arr[mid]!! == item) {
            return mid
        }
        if (arr[mid]!! > item) {
            right = mid - 1
        } else {
            left = mid + 1
        }
    }
    return 0 - 1
}

fun test_exponential_search(): Unit {
    var arr: MutableList<Int> = mutableListOf(0, 5, 7, 10, 15)
    expect(exponential_search(arr, 0) == 0)
    expect(exponential_search(arr, 15) == 4)
    expect(exponential_search(arr, 5) == 1)
    expect(exponential_search(arr, 6) == (0 - 1))
}

fun main() {
    test_exponential_search()
}
