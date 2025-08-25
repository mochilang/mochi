val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

data class Stack(var items: MutableList<Int> = mutableListOf<Int>(), var limit: Int = 0)
fun make_stack(limit: Int): Stack {
    return Stack(items = mutableListOf<Int>(), limit = limit)
}

fun is_empty(s: Stack): Boolean {
    return (s.items).size == 0
}

fun size(s: Stack): Int {
    return (s.items).size
}

fun is_full(s: Stack): Boolean {
    return (s.items).size >= s.limit
}

fun push(s: Stack, item: Int): Unit {
    if ((is_full(s)) as Boolean) {
        panic("stack overflow")
    }
    s.items = run { val _tmp = (s.items).toMutableList(); _tmp.add(item); _tmp }
}

fun pop(s: Stack): Int {
    if ((is_empty(s)) as Boolean) {
        panic("stack underflow")
    }
    var n: Int = ((s.items).size).toInt()
    var _val: Int = ((s.items)[n - 1]!!).toInt()
    s.items = _sliceList(s.items, 0, n - 1)
    return _val
}

fun peek(s: Stack): Int {
    if ((is_empty(s)) as Boolean) {
        panic("peek from empty stack")
    }
    return (s.items)[(s.items).size - 1]!!
}

fun contains(s: Stack, item: Int): Boolean {
    var i: Int = (0).toInt()
    while (i < (s.items).size) {
        if ((s.items)[i]!! == item) {
            return true
        }
        i = i + 1
    }
    return false
}

fun stack_repr(s: Stack): String {
    return s.items.toString()
}

fun user_main(): Unit {
    var s: Stack = make_stack(5)
    println(is_empty(s).toString())
    push(s, 0)
    push(s, 1)
    push(s, 2)
    println(_numToStr(peek(s)))
    println(_numToStr(size(s)))
    println(is_full(s).toString())
    push(s, 3)
    push(s, 4)
    println(is_full(s).toString())
    println(stack_repr(s))
    println(_numToStr(pop(s)))
    println(_numToStr(peek(s)))
    println(contains(s, 1).toString())
    println(contains(s, 9).toString())
}

fun main() {
    user_main()
}
