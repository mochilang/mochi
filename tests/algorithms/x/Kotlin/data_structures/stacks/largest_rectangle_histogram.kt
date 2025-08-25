import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun largest_rectangle_area(heights: MutableList<Int>): Int {
    var stack: MutableList<Int> = mutableListOf<Int>()
    var max_area: Int = (0).toInt()
    var hs: MutableList<Int> = heights
    hs = run { val _tmp = hs.toMutableList(); _tmp.add(0); _tmp }
    var i: Int = (0).toInt()
    while (i < hs.size) {
        while ((stack.size > 0) && (hs[i]!! < hs[stack[stack.size - 1]!!]!!)) {
            var top: Int = (stack[stack.size - 1]!!).toInt()
            stack = _sliceList(stack, 0, stack.size - 1)
            var height: Int = (hs[top]!!).toInt()
            var width: Int = (i).toInt()
            if (stack.size > 0) {
                width = (i - stack[stack.size - 1]!!) - 1
            }
            var area: Int = (height * width).toInt()
            if (area > max_area) {
                max_area = area
            }
        }
        stack = run { val _tmp = stack.toMutableList(); _tmp.add(i); _tmp }
        i = i + 1
    }
    return max_area
}

fun main() {
    println(_numToStr(largest_rectangle_area(mutableListOf(2, 1, 5, 6, 2, 3))))
    println(_numToStr(largest_rectangle_area(mutableListOf(2, 4))))
    println(_numToStr(largest_rectangle_area(mutableListOf(6, 2, 5, 4, 5, 1, 6))))
    println(_numToStr(largest_rectangle_area(mutableListOf(1))))
}
