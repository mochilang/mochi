import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun selection_sort(arr: MutableList<Int>): MutableList<Int> {
    var n: Int = (arr.size).toInt()
    var i: Int = (0).toInt()
    while (i < (n - 1)) {
        var min_index: Int = (i).toInt()
        var k: BigInteger = ((i + 1).toBigInteger())
        while (k.compareTo((n).toBigInteger()) < 0) {
            if (arr[(k).toInt()]!! < arr[min_index]!!) {
                min_index = (k.toInt())
            }
            k = k.add((1).toBigInteger())
        }
        if (min_index != i) {
            var tmp: Int = (arr[i]!!).toInt()
            _listSet(arr, i, arr[min_index]!!)
            _listSet(arr, min_index, tmp)
        }
        i = i + 1
    }
    return arr
}

fun main() {
    println(selection_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
    println(selection_sort(mutableListOf<Int>()).toString())
    println(selection_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45)).toString())
}
