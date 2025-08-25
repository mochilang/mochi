val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/matrix"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun unique(nums: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < nums.size) {
        var v: Int = (nums[i]!!).toInt()
        var found: Boolean = false
        var j: Int = (0).toInt()
        while (j < res.size) {
            if (res[j]!! == v) {
                found = true
                break
            }
            j = ((j).toLong() + (1).toLong()).toInt()
        }
        if (!found) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(v); _tmp }
        }
        i = ((i).toLong() + (1).toLong()).toInt()
    }
    return res
}

fun array_equalization(vector: MutableList<Int>, step_size: Int): Int {
    if (step_size <= 0) {
        error("Step size must be positive and non-zero.")
    }
    var elems: MutableList<Int> = unique(vector)
    var min_updates: Int = (vector.size).toInt()
    var i: Int = (0).toInt()
    while (i < elems.size) {
        var target: Int = (elems[i]!!).toInt()
        var idx: Int = (0).toInt()
        var updates: Int = (0).toInt()
        while (idx < vector.size) {
            if (vector[idx]!! != target) {
                updates = ((updates).toLong() + (1).toLong()).toInt()
                idx = ((idx).toLong() + (step_size).toLong()).toInt()
            } else {
                idx = ((idx).toLong() + (1).toLong()).toInt()
            }
        }
        if (updates < min_updates) {
            min_updates = updates
        }
        i = ((i).toLong() + (1).toLong()).toInt()
    }
    return min_updates
}

fun main() {
    println(_numToStr(array_equalization(mutableListOf(1, 1, 6, 2, 4, 6, 5, 1, 7, 2, 2, 1, 7, 2, 2), 4)))
    println(_numToStr(array_equalization(mutableListOf(22, 81, 88, 71, 22, 81, 632, 81, 81, 22, 92), 2)))
    println(_numToStr(array_equalization(mutableListOf(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 5)))
    println(_numToStr(array_equalization(mutableListOf(22, 22, 22, 33, 33, 33), 2)))
    println(_numToStr(array_equalization(mutableListOf(1, 2, 3), 2147483647)))
}
