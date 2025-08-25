val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/stacks"

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class StackWithQueues(var main_queue: MutableList<Int> = mutableListOf<Int>(), var temp_queue: MutableList<Int> = mutableListOf<Int>())
var stack: StackWithQueues = make_stack()
fun make_stack(): StackWithQueues {
    return StackWithQueues(main_queue = mutableListOf<Int>(), temp_queue = mutableListOf<Int>())
}

fun push(s: StackWithQueues, item: Int): Unit {
    s.temp_queue = run { val _tmp = (s.temp_queue).toMutableList(); _tmp.add(item); _tmp }
    while ((s.main_queue).size > 0) {
        s.temp_queue = run { val _tmp = (s.temp_queue).toMutableList(); _tmp.add((s.main_queue)[0]!!); _tmp }
        s.main_queue = _sliceList(s.main_queue, 1, (s.main_queue).size)
    }
    var new_main: MutableList<Int> = s.temp_queue
    s.temp_queue = s.main_queue
    s.main_queue = new_main
}

fun pop(s: StackWithQueues): Int {
    if ((s.main_queue).size == 0) {
        panic("pop from empty stack")
    }
    var item: Int = ((s.main_queue)[0]!!).toInt()
    s.main_queue = _sliceList(s.main_queue, 1, (s.main_queue).size)
    return item
}

fun peek(s: StackWithQueues): Int {
    if ((s.main_queue).size == 0) {
        panic("peek from empty stack")
    }
    return (s.main_queue)[0]!!
}

fun is_empty(s: StackWithQueues): Boolean {
    return (s.main_queue).size == 0
}

fun main() {
    push(stack, 1)
    push(stack, 2)
    push(stack, 3)
    println(_numToStr(peek(stack)))
    println(_numToStr(pop(stack)))
    println(_numToStr(peek(stack)))
    println(_numToStr(pop(stack)))
    println(_numToStr(pop(stack)))
    println(is_empty(stack).toString())
}
