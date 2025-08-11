fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun bubble_sort_iterative(collection: MutableList<Int>): MutableList<Int> {
    var n: Int = (collection.size).toInt()
    while (n > 0) {
        var swapped: Boolean = false
        var j: Int = (0).toInt()
        while (j < (n - 1)) {
            if (collection[j]!! > collection[j + 1]!!) {
                var temp: Int = (collection[j]!!).toInt()
                _listSet(collection, j, collection[j + 1]!!)
                _listSet(collection, j + 1, temp)
                swapped = true
            }
            j = j + 1
        }
        if (!swapped) {
            break
        }
        n = n - 1
    }
    return collection
}

fun bubble_sort_recursive(collection: MutableList<Int>): MutableList<Int> {
    var n: Int = (collection.size).toInt()
    var swapped: Boolean = false
    var i: Int = (0).toInt()
    while (i < (n - 1)) {
        if (collection[i]!! > collection[i + 1]!!) {
            var temp: Int = (collection[i]!!).toInt()
            _listSet(collection, i, collection[i + 1]!!)
            _listSet(collection, i + 1, temp)
            swapped = true
        }
        i = i + 1
    }
    if ((swapped as Boolean)) {
        return bubble_sort_recursive(collection)
    }
    return collection
}

fun copy_list(xs: MutableList<Int>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < xs.size) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return out
}

fun list_eq(a: MutableList<Int>, b: MutableList<Int>): Boolean {
    if (a.size != b.size) {
        return false
    }
    var k: Int = (0).toInt()
    while (k < a.size) {
        if (a[k]!! != b[k]!!) {
            return false
        }
        k = k + 1
    }
    return true
}

fun test_bubble_sort(): Unit {
    var example: MutableList<Int> = mutableListOf(0, 5, 2, 3, 2)
    var expected: MutableList<Int> = mutableListOf(0, 2, 2, 3, 5)
    if (!list_eq(bubble_sort_iterative(copy_list(example)), expected)) {
        panic("iterative failed")
    }
    if (!list_eq(bubble_sort_recursive(copy_list(example)), expected)) {
        panic("recursive failed")
    }
    var empty: MutableList<Int> = mutableListOf<Int>()
    if ((bubble_sort_iterative(copy_list(empty))).size != 0) {
        panic("empty iterative failed")
    }
    if ((bubble_sort_recursive(copy_list(empty))).size != 0) {
        panic("empty recursive failed")
    }
}

fun user_main(): Unit {
    test_bubble_sort()
    var arr: MutableList<Int> = mutableListOf(5, 1, 4, 2, 8)
    println(bubble_sort_iterative(copy_list(arr)).toString())
    println(bubble_sort_recursive(copy_list(arr)).toString())
}

fun main() {
    user_main()
}
