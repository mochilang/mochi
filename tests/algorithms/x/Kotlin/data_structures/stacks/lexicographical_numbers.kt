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

fun lexical_order(max_number: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var stack: MutableList<Int> = mutableListOf<Int>(1)
    while (stack.size > 0) {
        var idx: Int = (stack.size - 1).toInt()
        var num: Int = (stack[idx]!!).toInt()
        stack = _sliceList(stack, 0, idx)
        if (num > max_number) {
            continue
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(num); _tmp }
        if ((Math.floorMod(num, 10)) != 9) {
            stack = run { val _tmp = stack.toMutableList(); _tmp.add(num + 1); _tmp }
        }
        stack = run { val _tmp = stack.toMutableList(); _tmp.add(num * 10); _tmp }
    }
    return result
}

fun join_ints(xs: MutableList<Int>): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i > 0) {
            res = res + " "
        }
        res = res + _numToStr(xs[i]!!)
        i = i + 1
    }
    return res
}

fun main() {
    println(join_ints(lexical_order(13)))
    println(lexical_order(1).toString())
    println(join_ints(lexical_order(20)))
    println(join_ints(lexical_order(25)))
    println(lexical_order(12).toString())
}
