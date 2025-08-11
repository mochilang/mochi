fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun linear_search(sequence: MutableList<Int>, target: Int): Int {
    var i: Int = (0).toInt()
    while (i < sequence.size) {
        if (sequence[i]!! == target) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun rec_linear_search(sequence: MutableList<Int>, low: Int, high: Int, target: Int): Int {
    if (!(((((((0 <= high) && (high < sequence.size) as Boolean)) && (0 <= low) as Boolean)) && (low < sequence.size)) as Boolean)) {
        panic("Invalid upper or lower bound!")
    }
    if (high < low) {
        return 0 - 1
    }
    if (sequence[low]!! == target) {
        return low
    }
    if (sequence[high]!! == target) {
        return high
    }
    return rec_linear_search(sequence, low + 1, high - 1, target)
}

fun main() {
    println(linear_search(mutableListOf(0, 5, 7, 10, 15), 0).toString())
    println(linear_search(mutableListOf(0, 5, 7, 10, 15), 15).toString())
    println(linear_search(mutableListOf(0, 5, 7, 10, 15), 5).toString())
    println(linear_search(mutableListOf(0, 5, 7, 10, 15), 6).toString())
    println(rec_linear_search(mutableListOf(0, 30, 500, 100, 700), 0, 4, 0).toString())
    println(rec_linear_search(mutableListOf(0, 30, 500, 100, 700), 0, 4, 700).toString())
    println(rec_linear_search(mutableListOf(0, 30, 500, 100, 700), 0, 4, 30).toString())
    println(rec_linear_search(mutableListOf(0, 30, 500, 100, 700), 0, 4, 0 - 6).toString())
}
