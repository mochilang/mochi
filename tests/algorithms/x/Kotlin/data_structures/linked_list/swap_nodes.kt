val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class LinkedList(var data: MutableList<Int> = mutableListOf<Int>())
fun empty_list(): LinkedList {
    return LinkedList(data = mutableListOf<Int>())
}

fun push(list: LinkedList, value: Int): LinkedList {
    var res: MutableList<Int> = mutableListOf(value)
    res = concat(res, list.data)
    return LinkedList(data = res)
}

fun swap_nodes(list: LinkedList, v1: Int, v2: Int): LinkedList {
    if (v1 == v2) {
        return list
    }
    var idx1: Int = (0 - 1).toInt()
    var idx2: Int = (0 - 1).toInt()
    var i: Int = (0).toInt()
    while (i < (list.data).size) {
        if (((list.data)[i]!! == v1) && (idx1 == (0 - 1))) {
            idx1 = i
        }
        if (((list.data)[i]!! == v2) && (idx2 == (0 - 1))) {
            idx2 = i
        }
        i = i + 1
    }
    if ((idx1 == (0 - 1)) || (idx2 == (0 - 1))) {
        return list
    }
    var res: MutableList<Int> = list.data
    var temp: Int = (res[idx1]!!).toInt()
    _listSet(res, idx1, res[idx2]!!)
    _listSet(res, idx2, temp)
    return LinkedList(data = res)
}

fun to_string(list: LinkedList): String {
    return list.data.toString()
}

fun user_main(): Unit {
    var ll: LinkedList = empty_list()
    var i: Int = (5).toInt()
    while (i > 0) {
        ll = push(ll, i)
        i = i - 1
    }
    println("Original Linked List: " + to_string(ll))
    ll = swap_nodes(ll, 1, 4)
    println("Modified Linked List: " + to_string(ll))
    println("After swapping the nodes whose data is 1 and 4.")
}

fun main() {
    user_main()
}
