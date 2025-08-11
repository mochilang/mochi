fun concat(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    for (x in a) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(x); _tmp }
    }
    for (x in b) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(x); _tmp }
    }
    return result
}

fun quick_sort(data: MutableList<Int>): MutableList<Int> {
    if (data.size <= 1) {
        return data
    }
    var pivot: Int = (data[0]!!).toInt()
    var left: MutableList<Int> = mutableListOf<Int>()
    var right: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i < data.size) {
        var e: Int = (data[i]!!).toInt()
        if (e <= pivot) {
            left = run { val _tmp = left.toMutableList(); _tmp.add(e); _tmp }
        } else {
            right = run { val _tmp = right.toMutableList(); _tmp.add(e); _tmp }
        }
        i = i + 1
    }
    var sorted_left: MutableList<Int> = quick_sort(left)
    var sorted_right: MutableList<Int> = quick_sort(right)
    var left_pivot = run { val _tmp = sorted_left.toMutableList(); _tmp.add(pivot); _tmp }
    return concat(left_pivot, sorted_right)
}

fun main() {
    println(quick_sort(mutableListOf(2, 1, 0)).toString())
    println(quick_sort(mutableListOf(3, 5, 2, 4, 1)).toString())
}
