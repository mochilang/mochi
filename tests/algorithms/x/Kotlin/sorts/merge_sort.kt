fun subarray(xs: MutableList<Int>, start: Int, end: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (start).toInt()
    while (i < end) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return result
}

fun merge(left: MutableList<Int>, right: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var j: Int = (0).toInt()
    while ((i < left.size) && (j < right.size)) {
        if (left[i]!! <= right[j]!!) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(left[i]!!); _tmp }
            i = i + 1
        } else {
            result = run { val _tmp = result.toMutableList(); _tmp.add(right[j]!!); _tmp }
            j = j + 1
        }
    }
    while (i < left.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(left[i]!!); _tmp }
        i = i + 1
    }
    while (j < right.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(right[j]!!); _tmp }
        j = j + 1
    }
    return result
}

fun merge_sort(collection: MutableList<Int>): MutableList<Int> {
    if (collection.size <= 1) {
        return collection
    }
    var mid_index: Int = (collection.size / 2).toInt()
    var left: MutableList<Int> = subarray(collection, 0, mid_index)
    var right: MutableList<Int> = subarray(collection, mid_index, collection.size)
    var sorted_left: MutableList<Int> = merge_sort(left)
    var sorted_right: MutableList<Int> = merge_sort(right)
    return merge(sorted_left, sorted_right)
}

fun main() {
    println(merge_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
    println(merge_sort(mutableListOf<Int>()).toString())
    println(merge_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45)).toString())
}
