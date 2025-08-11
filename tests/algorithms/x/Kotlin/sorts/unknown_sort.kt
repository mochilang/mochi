import java.math.BigInteger

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun list_min(xs: MutableList<Int>): Int {
    var i: Int = (1).toInt()
    var m: Int = (xs[0]!!).toInt()
    while (i < xs.size) {
        if (xs[i]!! < m) {
            m = xs[i]!!
        }
        i = i + 1
    }
    return m
}

fun list_max(xs: MutableList<Int>): Int {
    var i: Int = (1).toInt()
    var m: Int = (xs[0]!!).toInt()
    while (i < xs.size) {
        if (xs[i]!! > m) {
            m = xs[i]!!
        }
        i = i + 1
    }
    return m
}

fun remove_once(xs: MutableList<Int>, value: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var removed: Boolean = false
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if ((!removed as Boolean) && (xs[i]!! == value)) {
            removed = true
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun reverse_list(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: BigInteger = ((xs.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[(i).toInt()]!!); _tmp }
        i = i.subtract((1).toBigInteger())
    }
    return res
}

fun merge_sort(collection: MutableList<Int>): MutableList<Int> {
    var start: MutableList<Int> = mutableListOf<Int>()
    var end: MutableList<Int> = mutableListOf<Int>()
    var coll: MutableList<Int> = collection
    while (coll.size > 1) {
        var mn: Int = (list_min(coll)).toInt()
        var mx: Int = (list_max(coll)).toInt()
        start = run { val _tmp = start.toMutableList(); _tmp.add(mn); _tmp }
        end = run { val _tmp = end.toMutableList(); _tmp.add(mx); _tmp }
        coll = remove_once(coll, mn)
        coll = remove_once(coll, mx)
    }
    end = reverse_list(end)
    return ((concat(concat(start, coll), end)) as MutableList<Int>)
}

fun test_merge_sort(): Unit {
    if (merge_sort(mutableListOf(0, 5, 3, 2, 2)) != mutableListOf(0, 2, 2, 3, 5)) {
        panic("case1 failed")
    }
    if (merge_sort(mutableListOf<Int>()) != mutableListOf<Any?>()) {
        panic("case2 failed")
    }
    if (merge_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45)) != mutableListOf(0 - 45, 0 - 5, 0 - 2)) {
        panic("case3 failed")
    }
}

fun user_main(): Unit {
    test_merge_sort()
    println(merge_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
}

fun main() {
    user_main()
}
