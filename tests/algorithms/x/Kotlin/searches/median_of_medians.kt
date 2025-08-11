import java.math.BigInteger

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun set_at_int(xs: MutableList<Int>, idx: Int, value: Int): MutableList<Int> {
    var i: Int = (0).toInt()
    var res: MutableList<Int> = mutableListOf<Int>()
    while (i < xs.size) {
        if (i == idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun sort_int(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = xs
    var i: Int = (1).toInt()
    while (i < res.size) {
        var key: Int = (res[i]!!).toInt()
        var j: BigInteger = ((i - 1).toBigInteger())
        while ((j.compareTo((0).toBigInteger()) >= 0) && (res[(j).toInt()]!! > key)) {
            res = set_at_int(res, ((j.add((1).toBigInteger())).toInt()), res[(j).toInt()]!!)
            j = j.subtract((1).toBigInteger())
        }
        res = set_at_int(res, ((j.add((1).toBigInteger())).toInt()), key)
        i = i + 1
    }
    return res
}

fun median_of_five(arr: MutableList<Int>): Int {
    var sorted: MutableList<Int> = sort_int(arr)
    return sorted[sorted.size / 2]!!
}

fun median_of_medians(arr: MutableList<Int>): Int {
    if (arr.size <= 5) {
        return median_of_five(arr)
    }
    var medians: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < arr.size) {
        if ((i + 5) <= arr.size) {
            medians = run { val _tmp = medians.toMutableList(); _tmp.add(median_of_five(_sliceList(arr, i, i + 5))); _tmp }
        } else {
            medians = run { val _tmp = medians.toMutableList(); _tmp.add(median_of_five(_sliceList(arr, i, arr.size))); _tmp }
        }
        i = i + 5
    }
    return median_of_medians(medians)
}

fun quick_select(arr: MutableList<Int>, target: Int): Int {
    if (target > arr.size) {
        return 0 - 1
    }
    var x: Int = (median_of_medians(arr)).toInt()
    var left: MutableList<Int> = mutableListOf<Int>()
    var right: MutableList<Int> = mutableListOf<Int>()
    var check: Boolean = false
    var i: Int = (0).toInt()
    while (i < arr.size) {
        if (arr[i]!! < x) {
            left = run { val _tmp = left.toMutableList(); _tmp.add(arr[i]!!); _tmp }
        } else {
            if (arr[i]!! > x) {
                right = run { val _tmp = right.toMutableList(); _tmp.add(arr[i]!!); _tmp }
            } else {
                if (arr[i]!! == x) {
                    if (!check) {
                        check = true
                    } else {
                        right = run { val _tmp = right.toMutableList(); _tmp.add(arr[i]!!); _tmp }
                    }
                } else {
                    right = run { val _tmp = right.toMutableList(); _tmp.add(arr[i]!!); _tmp }
                }
            }
        }
        i = i + 1
    }
    var rank_x: Int = (left.size + 1).toInt()
    var answer: Int = (0).toInt()
    if (rank_x == target) {
        answer = x
    } else {
        if (rank_x > target) {
            answer = quick_select(left, target)
        } else {
            answer = quick_select(right, target - rank_x)
        }
    }
    return answer
}

fun user_main(): Unit {
    println(median_of_five(mutableListOf(5, 4, 3, 2)).toString())
    println(quick_select(mutableListOf(2, 4, 5, 7, 899, 54, 32), 5).toString())
}

fun main() {
    user_main()
}
