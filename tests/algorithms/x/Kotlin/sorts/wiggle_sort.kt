import java.math.BigInteger

fun swap(xs: MutableList<Double>, i: Int, j: Int): MutableList<Double> {
    var res: MutableList<Double> = mutableListOf<Double>()
    var k: Int = (0).toInt()
    while (k < xs.size) {
        if (k == i) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[j]!!); _tmp }
        } else {
            if (k == j) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
            } else {
                res = run { val _tmp = res.toMutableList(); _tmp.add(xs[k]!!); _tmp }
            }
        }
        k = k + 1
    }
    return res
}

fun wiggle_sort(nums: MutableList<Double>): MutableList<Double> {
    var i: Int = (0).toInt()
    var res: MutableList<Double> = nums
    while (i < res.size) {
        var j = if (i == 0) res.size - 1 else i - 1
        var prev: Double = res[(j).toInt()]!!
        var curr: Double = res[i]!!
        if (((Math.floorMod(i, 2)) == 1) == (prev > curr)) {
            res = swap(res, (j.toInt()), i)
        }
        i = i + 1
    }
    return res
}

fun main() {
    println(wiggle_sort(mutableListOf(3.0, 5.0, 2.0, 1.0, 6.0, 4.0)).toString())
    println(wiggle_sort(mutableListOf(0.0, 5.0, 3.0, 2.0, 2.0)).toString())
    println(wiggle_sort(mutableListOf<Double>()).toString())
    println(wiggle_sort(mutableListOf(0.0 - 2.0, 0.0 - 5.0, 0.0 - 45.0)).toString())
    println(wiggle_sort(mutableListOf(0.0 - 2.1, 0.0 - 5.68, 0.0 - 45.11)).toString())
}
