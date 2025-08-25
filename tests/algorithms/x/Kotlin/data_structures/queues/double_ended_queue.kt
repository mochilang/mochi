val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class Deque(var data: MutableList<Int> = mutableListOf<Int>())
data class PopResult(var deque: Deque = Deque(data = mutableListOf<Int>()), var value: Int = 0)
fun empty_deque(): Deque {
    return Deque(data = mutableListOf<Int>())
}

fun push_back(dq: Deque, value: Int): Deque {
    return Deque(data = run { val _tmp = (dq.data).toMutableList(); _tmp.add(value); _tmp })
}

fun push_front(dq: Deque, value: Int): Deque {
    var res: MutableList<Int> = mutableListOf(value)
    var i: Int = (0).toInt()
    while (i < (dq.data).size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((dq.data)[i]!!); _tmp }
        i = i + 1
    }
    return Deque(data = res)
}

fun extend_back(dq: Deque, values: MutableList<Int>): Deque {
    var res: MutableList<Int> = dq.data
    var i: Int = (0).toInt()
    while (i < values.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(values[i]!!); _tmp }
        i = i + 1
    }
    return Deque(data = res)
}

fun extend_front(dq: Deque, values: MutableList<Int>): Deque {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (values.size - 1).toInt()
    while (i >= 0) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(values[i]!!); _tmp }
        i = i - 1
    }
    var j: Int = (0).toInt()
    while (j < (dq.data).size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((dq.data)[j]!!); _tmp }
        j = j + 1
    }
    return Deque(data = res)
}

fun pop_back(dq: Deque): PopResult {
    if ((dq.data).size == 0) {
        panic("pop from empty deque")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < ((dq.data).size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((dq.data)[i]!!); _tmp }
        i = i + 1
    }
    return PopResult(deque = Deque(data = res), value = (dq.data)[(dq.data).size - 1]!!)
}

fun pop_front(dq: Deque): PopResult {
    if ((dq.data).size == 0) {
        panic("popleft from empty deque")
    }
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i < (dq.data).size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((dq.data)[i]!!); _tmp }
        i = i + 1
    }
    return PopResult(deque = Deque(data = res), value = (dq.data)[0]!!)
}

fun is_empty(dq: Deque): Boolean {
    return (dq.data).size == 0
}

fun length(dq: Deque): Int {
    return (dq.data).size
}

fun to_string(dq: Deque): String {
    if ((dq.data).size == 0) {
        return "[]"
    }
    var s: String = "[" + _numToStr((dq.data)[0]!!)
    var i: Int = (1).toInt()
    while (i < (dq.data).size) {
        s = (s + ", ") + _numToStr((dq.data)[i]!!)
        i = i + 1
    }
    return s + "]"
}

fun user_main(): Unit {
    var dq: Deque = empty_deque()
    dq = push_back(dq, 2)
    dq = push_front(dq, 1)
    dq = extend_back(dq, mutableListOf(3, 4))
    dq = extend_front(dq, mutableListOf(0))
    println(to_string(dq))
    var r: PopResult = pop_back(dq)
    dq = r.deque
    println(r.value)
    r = pop_front(dq)
    dq = r.deque
    println(r.value)
    println(to_string(dq))
    println(is_empty(empty_deque()))
}

fun main() {
    user_main()
}
