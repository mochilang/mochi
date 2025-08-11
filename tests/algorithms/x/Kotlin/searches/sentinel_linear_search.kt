fun remove_last(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun sentinel_linear_search(sequence: MutableList<Int>, target: Int): Int {
    var seq: MutableList<Int> = sequence
    seq = run { val _tmp = seq.toMutableList(); _tmp.add(target); _tmp }
    var index: Int = (0).toInt()
    while (seq[index]!! != target) {
        index = index + 1
    }
    seq = remove_last(seq)
    if (index == seq.size) {
        return 0 - 1
    }
    return index
}

fun main() {
    println(sentinel_linear_search(mutableListOf(0, 5, 7, 10, 15), 0).toString())
    println(sentinel_linear_search(mutableListOf(0, 5, 7, 10, 15), 15).toString())
    println(sentinel_linear_search(mutableListOf(0, 5, 7, 10, 15), 5).toString())
    println(sentinel_linear_search(mutableListOf(0, 5, 7, 10, 15), 6).toString())
}
