fun set_at_int(xs: MutableList<Int>, idx: Int, value: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
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

fun comp_and_swap(arr: MutableList<Int>, i: Int, j: Int, dir: Int): MutableList<Int> {
    var res: MutableList<Int> = arr
    var xi: Int = (arr[i]!!).toInt()
    var xj: Int = (arr[j]!!).toInt()
    if ((((dir == 1) && (xi > xj) as Boolean)) || (((dir == 0) && (xi < xj) as Boolean))) {
        res = set_at_int(res, i, xj)
        res = set_at_int(res, j, xi)
    }
    return res
}

fun bitonic_merge(arr: MutableList<Int>, low: Int, length: Int, dir: Int): MutableList<Int> {
    var res: MutableList<Int> = arr
    if (length > 1) {
        var mid: Int = (length / 2).toInt()
        var k: Int = (low).toInt()
        while (k < (low + mid)) {
            res = comp_and_swap(res, k, k + mid, dir)
            k = k + 1
        }
        res = bitonic_merge(res, low, mid, dir)
        res = bitonic_merge(res, low + mid, mid, dir)
    }
    return res
}

fun bitonic_sort(arr: MutableList<Int>, low: Int, length: Int, dir: Int): MutableList<Int> {
    var res: MutableList<Int> = arr
    if (length > 1) {
        var mid: Int = (length / 2).toInt()
        res = bitonic_sort(res, low, mid, 1)
        res = bitonic_sort(res, low + mid, mid, 0)
        res = bitonic_merge(res, low, length, dir)
    }
    return res
}

fun user_main(): Unit {
    var data: MutableList<Int> = mutableListOf(12, 34, 92, 0 - 23, 0, 0 - 121, 0 - 167, 145)
    var asc: MutableList<Int> = bitonic_sort(data, 0, data.size, 1)
    println(asc.toString())
    var desc: MutableList<Int> = bitonic_merge(asc, 0, asc.size, 0)
    println(desc.toString())
}

fun main() {
    user_main()
}
