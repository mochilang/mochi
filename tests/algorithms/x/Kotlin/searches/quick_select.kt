fun partition(data: MutableList<Int>, pivot: Int): MutableList<MutableList<Int>> {
    var less: MutableList<Int> = mutableListOf<Int>()
    var equal: MutableList<Int> = mutableListOf<Int>()
    var greater: MutableList<Int> = mutableListOf<Int>()
    for (i in 0 until data.size) {
        var v: Int = (data[i]!!).toInt()
        if (v < pivot) {
            less = run { val _tmp = less.toMutableList(); _tmp.add(v); _tmp }
        } else {
            if (v > pivot) {
                greater = run { val _tmp = greater.toMutableList(); _tmp.add(v); _tmp }
            } else {
                equal = run { val _tmp = equal.toMutableList(); _tmp.add(v); _tmp }
            }
        }
    }
    return mutableListOf(less, equal, greater)
}

fun quick_select(items: MutableList<Int>, index: Int): Int {
    if ((index < 0) || (index >= items.size)) {
        return 0 - 1
    }
    var pivot: Int = (items[items.size / 2]!!).toInt()
    var parts: MutableList<MutableList<Int>> = partition(items, pivot)
    var smaller: MutableList<Int> = parts[0]!!
    var equal: MutableList<Int> = parts[1]!!
    var larger: MutableList<Int> = parts[2]!!
    var count: Int = (equal.size).toInt()
    var m: Int = (smaller.size).toInt()
    if ((m <= index) && (index < (m + count))) {
        return pivot
    } else {
        if (index < m) {
            return quick_select(smaller, index)
        } else {
            return quick_select(larger, index - (m + count))
        }
    }
}

fun median(items: MutableList<Int>): Double {
    var n: Int = (items.size).toInt()
    var mid: Int = (n / 2).toInt()
    if ((Math.floorMod(n, 2)) != 0) {
        return 1.0 * quick_select(items, mid)
    } else {
        var low: Int = (quick_select(items, mid - 1)).toInt()
        var high: Int = (quick_select(items, mid)).toInt()
        return (1.0 * (low + high)) / 2.0
    }
}

fun main() {
    println(quick_select(mutableListOf(2, 4, 5, 7, 899, 54, 32), 5).toString())
    println(quick_select(mutableListOf(2, 4, 5, 7, 899, 54, 32), 1).toString())
    println(quick_select(mutableListOf(5, 4, 3, 2), 2).toString())
    println(quick_select(mutableListOf(3, 5, 7, 10, 2, 12), 3).toString())
    println(median(mutableListOf(3, 2, 2, 9, 9)).toString())
    println(median(mutableListOf(2, 2, 9, 9, 9, 3)).toString())
}
