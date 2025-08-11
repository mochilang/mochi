import java.math.BigInteger

fun interpolation_search(arr: MutableList<Int>, item: Int): Int {
    var left: Int = (0).toInt()
    var right: BigInteger = ((arr.size - 1).toBigInteger())
    while ((left).toBigInteger().compareTo((right)) <= 0) {
        if (arr[left]!! == arr[(right).toInt()]!!) {
            if (arr[left]!! == item) {
                return left
            }
            return 0 - 1
        }
        var point = (left).toBigInteger().add(((((item - arr[left]!!)).toBigInteger().multiply((right.subtract((left).toBigInteger())))).divide((arr[(right).toInt()]!! - arr[left]!!).toBigInteger())))
        if ((point.compareTo((0).toBigInteger()) < 0) || (point.compareTo((arr.size).toBigInteger()) >= 0)) {
            return 0 - 1
        }
        var current: Int = (arr[(point).toInt()]!!).toInt()
        if (current == item) {
            return (point.toInt())
        }
        if (point.compareTo((left).toBigInteger()) < 0) {
            right = (left.toBigInteger())
            left = (point.toInt())
        } else {
            if (point.compareTo((right)) > 0) {
                left = (right.toInt())
                right = point
            } else {
                if (item < current) {
                    right = point.subtract((1).toBigInteger())
                } else {
                    left = ((point.add((1).toBigInteger())).toInt())
                }
            }
        }
    }
    return 0 - 1
}

fun interpolation_search_recursive(arr: MutableList<Int>, item: Int, left: Int, right: Int): Int {
    if (left > right) {
        return 0 - 1
    }
    if (arr[left]!! == arr[right]!!) {
        if (arr[left]!! == item) {
            return left
        }
        return 0 - 1
    }
    var point: Int = (left + (((item - arr[left]!!) * (right - left)) / (arr[right]!! - arr[left]!!))).toInt()
    if ((point < 0) || (point >= arr.size)) {
        return 0 - 1
    }
    if (arr[point]!! == item) {
        return point
    }
    if (point < left) {
        return interpolation_search_recursive(arr, item, point, left)
    }
    if (point > right) {
        return interpolation_search_recursive(arr, item, right, left)
    }
    if (arr[point]!! > item) {
        return interpolation_search_recursive(arr, item, left, point - 1)
    }
    return interpolation_search_recursive(arr, item, point + 1, right)
}

fun interpolation_search_by_recursion(arr: MutableList<Int>, item: Int): Int {
    return interpolation_search_recursive(arr, item, 0, arr.size - 1)
}

fun main() {
    println(interpolation_search(mutableListOf(1, 2, 3, 4, 5), 2).toString())
    println(interpolation_search(mutableListOf(1, 2, 3, 4, 5), 6).toString())
    println(interpolation_search_by_recursion(mutableListOf(0, 5, 7, 10, 15), 5).toString())
    println(interpolation_search_by_recursion(mutableListOf(0, 5, 7, 10, 15), 100).toString())
    println(interpolation_search_by_recursion(mutableListOf(5, 5, 5, 5, 5), 3).toString())
}
