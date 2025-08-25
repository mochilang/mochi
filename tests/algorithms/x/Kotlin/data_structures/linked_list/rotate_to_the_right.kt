import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun list_to_string(xs: MutableList<Int>): String {
    if (xs.size == 0) {
        return ""
    }
    var s: String = _numToStr(xs[0]!!)
    var i: Int = (1).toInt()
    while (i < xs.size) {
        s = (s + "->") + _numToStr(xs[i]!!)
        i = i + 1
    }
    return s
}

fun insert_node(xs: MutableList<Int>, data: Int): MutableList<Int> {
    return run { val _tmp = xs.toMutableList(); _tmp.add(data); _tmp }
}

fun rotate_to_the_right(xs: MutableList<Int>, places: Int): MutableList<Int> {
    if (xs.size == 0) {
        panic("The linked list is empty.")
    }
    var n: Int = (xs.size).toInt()
    var k: Int = (Math.floorMod(places, n)).toInt()
    if (k == 0) {
        return xs
    }
    var split: Int = (n - k).toInt()
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (split).toInt()
    while (i < n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    var j: Int = (0).toInt()
    while (j < split) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[j]!!); _tmp }
        j = j + 1
    }
    return res
}

fun user_main(): Unit {
    var head: MutableList<Int> = mutableListOf<Int>()
    head = insert_node(head, 5)
    head = insert_node(head, 1)
    head = insert_node(head, 2)
    head = insert_node(head, 4)
    head = insert_node(head, 3)
    println("Original list: " + list_to_string(head))
    var places: Int = (3).toInt()
    var new_head: MutableList<Int> = rotate_to_the_right(head, places)
    println((("After " + _numToStr(places)) + " iterations: ") + list_to_string(new_head))
}

fun main() {
    user_main()
}
