import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class SinglyLinkedList(var data: MutableList<Int> = mutableListOf<Int>())
data class DeleteResult(var list: SinglyLinkedList = SinglyLinkedList(data = mutableListOf<Int>()), var value: Int = 0)
fun empty_list(): SinglyLinkedList {
    return SinglyLinkedList(data = mutableListOf<Int>())
}

fun length(list: SinglyLinkedList): Int {
    return (list.data).size
}

fun is_empty(list: SinglyLinkedList): Boolean {
    return (list.data).size == 0
}

fun to_string(list: SinglyLinkedList): String {
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

fun insert_nth(list: SinglyLinkedList, index: Int, value: Int): SinglyLinkedList {
    if ((index < 0) || (index > (list.data).size)) {
        panic("index out of range")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < index) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        i = i + 1
    }
    res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
    while (i < (list.data).size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        i = i + 1
    }
    return SinglyLinkedList(data = res)
}

fun insert_head(list: SinglyLinkedList, value: Int): SinglyLinkedList {
    return insert_nth(list, 0, value)
}

fun insert_tail(list: SinglyLinkedList, value: Int): SinglyLinkedList {
    return insert_nth(list, (list.data).size, value)
}

fun delete_nth(list: SinglyLinkedList, index: Int): DeleteResult {
    if ((index < 0) || (index >= (list.data).size)) {
        panic("index out of range")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var _val: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < (list.data).size) {
        if (i == index) {
            _val = (list.data)[i]!!
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        }
        i = i + 1
    }
    return DeleteResult(list = SinglyLinkedList(data = res), value = _val)
}

fun delete_head(list: SinglyLinkedList): DeleteResult {
    return delete_nth(list, 0)
}

fun delete_tail(list: SinglyLinkedList): DeleteResult {
    return delete_nth(list, (list.data).size - 1)
}

fun get_item(list: SinglyLinkedList, index: Int): Int {
    if ((index < 0) || (index >= (list.data).size)) {
        panic("index out of range")
    }
    return (list.data)[index]!!
}

fun set_item(list: SinglyLinkedList, index: Int, value: Int): SinglyLinkedList {
    if ((index < 0) || (index >= (list.data).size)) {
        panic("index out of range")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < (list.data).size) {
        if (i == index) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        }
        i = i + 1
    }
    return SinglyLinkedList(data = res)
}

fun reverse_list(list: SinglyLinkedList): SinglyLinkedList {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = ((list.data).size - 1).toInt()
    while (i >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((list.data)[i]!!); _tmp }
        i = i - 1
    }
    return SinglyLinkedList(data = res)
}

fun user_main(): Unit {
    var lst: SinglyLinkedList = empty_list()
    var i: Int = (1).toInt()
    while (i <= 5) {
        lst = insert_tail(lst, i)
        i = i + 1
    }
    println(to_string(lst))
    lst = insert_head(lst, 0)
    println(to_string(lst))
    var del: DeleteResult = delete_head(lst)
    lst = del.list
    println(_numToStr(del.value))
    del = delete_tail(lst)
    lst = del.list
    println(_numToStr(del.value))
    del = delete_nth(lst, 2)
    lst = del.list
    println(_numToStr(del.value))
    lst = set_item(lst, 1, 99)
    println(_numToStr(get_item(lst, 1)))
    lst = reverse_list(lst)
    println(to_string(lst))
}

fun main() {
    user_main()
}
