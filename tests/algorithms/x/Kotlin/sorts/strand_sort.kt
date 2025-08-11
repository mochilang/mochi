fun merge(xs: MutableList<Int>, ys: MutableList<Int>, reverse: Boolean): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var j: Int = (0).toInt()
    while ((i < xs.size) && (j < ys.size)) {
        if ((reverse as Boolean)) {
            if (xs[i]!! > ys[j]!!) {
                result = run { val _tmp = result.toMutableList(); _tmp.add(xs[i]!!); _tmp }
                i = i + 1
            } else {
                result = run { val _tmp = result.toMutableList(); _tmp.add(ys[j]!!); _tmp }
                j = j + 1
            }
        } else {
            if (xs[i]!! < ys[j]!!) {
                result = run { val _tmp = result.toMutableList(); _tmp.add(xs[i]!!); _tmp }
                i = i + 1
            } else {
                result = run { val _tmp = result.toMutableList(); _tmp.add(ys[j]!!); _tmp }
                j = j + 1
            }
        }
    }
    while (i < xs.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    while (j < ys.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(ys[j]!!); _tmp }
        j = j + 1
    }
    return result
}

fun strand_sort_rec(arr: MutableList<Int>, reverse: Boolean, solution: MutableList<Int>): MutableList<Int> {
    var solution: MutableList<Int> = solution
    if (arr.size == 0) {
        return solution
    }
    var sublist: MutableList<Int> = mutableListOf<Int>()
    var remaining: MutableList<Int> = mutableListOf<Int>()
    sublist = run { val _tmp = sublist.toMutableList(); _tmp.add(arr[0]!!); _tmp }
    var last: Int = (arr[0]!!).toInt()
    var k: Int = (1).toInt()
    while (k < arr.size) {
        var item: Int = (arr[k]!!).toInt()
        if ((reverse as Boolean)) {
            if (item < last) {
                sublist = run { val _tmp = sublist.toMutableList(); _tmp.add(item); _tmp }
                last = item
            } else {
                remaining = run { val _tmp = remaining.toMutableList(); _tmp.add(item); _tmp }
            }
        } else {
            if (item > last) {
                sublist = run { val _tmp = sublist.toMutableList(); _tmp.add(item); _tmp }
                last = item
            } else {
                remaining = run { val _tmp = remaining.toMutableList(); _tmp.add(item); _tmp }
            }
        }
        k = k + 1
    }
    solution = merge(solution, sublist, reverse)
    return strand_sort_rec(remaining, reverse, solution)
}

fun strand_sort(arr: MutableList<Int>, reverse: Boolean): MutableList<Int> {
    return strand_sort_rec(arr, reverse, mutableListOf<Int>())
}

fun main() {
    println(strand_sort(mutableListOf(4, 3, 5, 1, 2), false).toString())
    println(strand_sort(mutableListOf(4, 3, 5, 1, 2), true).toString())
}
