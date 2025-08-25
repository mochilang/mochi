import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class LinkedList(var data: MutableList<Int> = mutableListOf<Int>())
fun to_string(list: LinkedList): String {
    if ((list.data).size == 0) {
        return ""
    }
    var s: String = _numToStr((list.data)[0]!!)
    var i: Int = (1).toInt()
    while (i < (list.data).size) {
        s = (s + " -> ") + _numToStr((list.data)[i]!!)
        i = i + 1
    }
    return s
}

fun reverse_k_nodes(list: LinkedList, k: Int): LinkedList {
    if (k <= 1) {
        return list
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < (list.data).size) {
        var j: Int = (0).toInt()
        var group: MutableList<Int> = mutableListOf<Int>()
        while ((j < k) && ((i + j) < (list.data).size)) {
            group = run { val _tmp = group.toMutableList(); _tmp.add((list.data)[i + j]!!); _tmp }
            j = j + 1
        }
        if (group.size == k) {
            var g: Int = (k - 1).toInt()
            while (g >= 0) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(group[g]!!); _tmp }
                g = g - 1
            }
        } else {
            var g: Int = (0).toInt()
            while (g < group.size) {
                res = run { val _tmp = res.toMutableList(); _tmp.add(group[g]!!); _tmp }
                g = g + 1
            }
        }
        i = i + k
    }
    return LinkedList(data = res)
}

fun user_main(): Unit {
    var ll: LinkedList = LinkedList(data = mutableListOf(1, 2, 3, 4, 5))
    println("Original Linked List: " + to_string(ll))
    var k: Int = (2).toInt()
    ll = reverse_k_nodes(ll, k)
    println((("After reversing groups of size " + _numToStr(k)) + ": ") + to_string(ll))
}

fun main() {
    user_main()
}
