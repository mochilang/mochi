fun is_sorted(arr: MutableList<Int>): Boolean {
    var i: Int = (1).toInt()
    while (i < arr.size) {
        if (arr[i - 1]!! > arr[i]!!) {
            return false
        }
        i = i + 1
    }
    return true
}

fun binary_search(sorted_collection: MutableList<Int>, item: Int): Int {
    if (!is_sorted(sorted_collection)) {
        return 0 - 1
    }
    var left: Int = (0).toInt()
    var right: Int = (sorted_collection.size - 1).toInt()
    while (left <= right) {
        var midpoint: Int = (left + ((right - left) / 2)).toInt()
        var current_item: Int = (sorted_collection[midpoint]!!).toInt()
        if (current_item == item) {
            return midpoint
        }
        if (item < current_item) {
            right = midpoint - 1
        } else {
            left = midpoint + 1
        }
    }
    return 0 - 1
}

fun binary_search_by_recursion(sorted_collection: MutableList<Int>, item: Int, left: Int, right: Int): Int {
    if (right < left) {
        return 0 - 1
    }
    var midpoint: Int = (left + ((right - left) / 2)).toInt()
    if (sorted_collection[midpoint]!! == item) {
        return midpoint
    }
    if (sorted_collection[midpoint]!! > item) {
        return binary_search_by_recursion(sorted_collection, item, left, midpoint - 1)
    }
    return binary_search_by_recursion(sorted_collection, item, midpoint + 1, right)
}

fun exponential_search(sorted_collection: MutableList<Int>, item: Int): Int {
    if (!is_sorted(sorted_collection)) {
        return 0 - 1
    }
    if (sorted_collection.size == 0) {
        return 0 - 1
    }
    var bound: Int = (1).toInt()
    while ((bound < sorted_collection.size) && (sorted_collection[bound]!! < item)) {
        bound = bound * 2
    }
    var left: Int = (bound / 2).toInt()
    var right: Int = (mutableListOf(bound, sorted_collection.size - 1).min()!!).toInt()
    return binary_search_by_recursion(sorted_collection, item, left, right)
}

fun user_main(): Unit {
    var data: MutableList<Int> = mutableListOf(0, 5, 7, 10, 15)
    println(binary_search(data, 0).toString())
    println(binary_search(data, 15).toString())
    println(binary_search(data, 5).toString())
    println(binary_search(data, 6).toString())
    println(binary_search_by_recursion(data, 0, 0, data.size - 1).toString())
    println(binary_search_by_recursion(data, 15, 0, data.size - 1).toString())
    println(binary_search_by_recursion(data, 5, 0, data.size - 1).toString())
    println(binary_search_by_recursion(data, 6, 0, data.size - 1).toString())
    println(exponential_search(data, 0).toString())
    println(exponential_search(data, 15).toString())
    println(exponential_search(data, 5).toString())
    println(exponential_search(data, 6).toString())
}

fun main() {
    user_main()
}
