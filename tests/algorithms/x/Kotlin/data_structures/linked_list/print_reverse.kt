import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class LinkedList(var data: MutableList<Int> = mutableListOf<Int>())
fun empty_list(): LinkedList {
    return LinkedList(data = mutableListOf<Int>())
}

fun append_value(list: LinkedList, value: Int): LinkedList {
    var d: MutableList<Int> = list.data
    d = run { val _tmp = d.toMutableList(); _tmp.add(value); _tmp }
    return LinkedList(data = d)
}

fun extend_list(list: LinkedList, items: MutableList<Int>): LinkedList {
    var result: LinkedList = list
    var i: Int = (0).toInt()
    while (i < items.size) {
        result = append_value(result, items[i]!!)
        i = i + 1
    }
    return result
}

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

fun make_linked_list(items: MutableList<Int>): LinkedList {
    if (items.size == 0) {
        panic("The Elements List is empty")
    }
    var ll: LinkedList = empty_list()
    ll = extend_list(ll, items)
    return ll
}

fun in_reverse(list: LinkedList): String {
    if ((list.data).size == 0) {
        return ""
    }
    var i: Int = ((list.data).size - 1).toInt()
    var s: String = _numToStr((list.data)[i]!!)
    i = i - 1
    while (i >= 0) {
        s = (s + " <- ") + _numToStr((list.data)[i]!!)
        i = i - 1
    }
    return s
}

fun user_main(): Unit {
    var linked_list: LinkedList = make_linked_list(mutableListOf(14, 52, 14, 12, 43))
    println("Linked List:  " + to_string(linked_list))
    println("Reverse List: " + in_reverse(linked_list))
}

fun main() {
    user_main()
}
