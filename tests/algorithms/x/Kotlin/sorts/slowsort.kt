import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var seq1: MutableList<Int> = mutableListOf(1, 6, 2, 5, 3, 4, 4, 5)
fun swap(seq: MutableList<Int>, i: Int, j: Int): Unit {
    var temp: Int = (seq[i]!!).toInt()
    _listSet(seq, i, seq[j]!!)
    _listSet(seq, j, temp)
}

fun slowsort_recursive(seq: MutableList<Int>, start: Int, end_index: Int): Unit {
    if (start >= end_index) {
        return
    }
    var mid: Int = ((start + end_index) / 2).toInt()
    slowsort_recursive(seq, start, mid)
    slowsort_recursive(seq, mid + 1, end_index)
    if (seq[end_index]!! < seq[mid]!!) {
        swap(seq, end_index, mid)
    }
    slowsort_recursive(seq, start, end_index - 1)
}

fun slow_sort(seq: MutableList<Int>): MutableList<Int> {
    if (seq.size > 0) {
        slowsort_recursive(seq, 0, seq.size - 1)
    }
    return seq
}

fun main() {
    println(slow_sort(seq1).toString())
    var seq2: MutableList<Int> = mutableListOf<Int>()
    println(slow_sort(seq2).toString())
    var seq3: MutableList<Int> = mutableListOf(2)
    println(slow_sort(seq3).toString())
    var seq4: MutableList<Int> = mutableListOf(1, 2, 3, 4)
    println(slow_sort(seq4).toString())
    var seq5: MutableList<Int> = mutableListOf(4, 3, 2, 1)
    println(slow_sort(seq5).toString())
    var seq6: MutableList<Int> = mutableListOf(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    slowsort_recursive(seq6, 2, 7)
    println(seq6.toString())
    var seq7: MutableList<Int> = mutableListOf(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    slowsort_recursive(seq7, 0, 4)
    println(seq7.toString())
    var seq8: MutableList<Int> = mutableListOf(9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
    slowsort_recursive(seq8, 5, seq8.size - 1)
    println(seq8.toString())
}
