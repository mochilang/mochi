fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun bead_sort(sequence: MutableList<Int>): MutableList<Int> {
    var n: Int = (sequence.size).toInt()
    var i: Int = (0).toInt()
    while (i < n) {
        if (sequence[i]!! < 0) {
            panic("Sequence must be list of non-negative integers")
        }
        i = i + 1
    }
    var pass: Int = (0).toInt()
    while (pass < n) {
        var j: Int = (0).toInt()
        while (j < (n - 1)) {
            var upper: Int = (sequence[j]!!).toInt()
            var lower: Int = (sequence[j + 1]!!).toInt()
            if (upper > lower) {
                var diff: Int = (upper - lower).toInt()
                _listSet(sequence, j, upper - diff)
                _listSet(sequence, j + 1, lower + diff)
            }
            j = j + 1
        }
        pass = pass + 1
    }
    return sequence
}

fun main() {
    println(bead_sort(mutableListOf(6, 11, 12, 4, 1, 5)).toString())
    println(bead_sort(mutableListOf(9, 8, 7, 6, 5, 4, 3, 2, 1)).toString())
    println(bead_sort(mutableListOf(5, 0, 4, 3)).toString())
    println(bead_sort(mutableListOf(8, 2, 1)).toString())
}
