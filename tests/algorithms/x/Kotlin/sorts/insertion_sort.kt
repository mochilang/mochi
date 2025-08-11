import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun insertion_sort(xs: MutableList<Int>): MutableList<Int> {
    var i: Int = (1).toInt()
    while (i < xs.size) {
        var value: Int = (xs[i]!!).toInt()
        var j: BigInteger = ((i - 1).toBigInteger())
        while ((j.compareTo((0).toBigInteger()) >= 0) && (xs[(j).toInt()]!! > value)) {
            _listSet(xs, (j.add((1).toBigInteger())).toInt(), xs[(j).toInt()]!!)
            j = j.subtract((1).toBigInteger())
        }
        _listSet(xs, (j.add((1).toBigInteger())).toInt(), value)
        i = i + 1
    }
    return xs
}

fun main() {
    println(insertion_sort(mutableListOf(0, 5, 3, 2, 2)).toString())
    println(insertion_sort(mutableListOf<Int>()).toString())
    println(insertion_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45)).toString())
    println(insertion_sort(mutableListOf(3)).toString())
}
