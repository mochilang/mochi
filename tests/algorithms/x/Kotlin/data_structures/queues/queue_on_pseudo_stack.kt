val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class Queue(var stack: MutableList<Int> = mutableListOf<Int>(), var length: Int = 0)
data class GetResult(var queue: Queue = Queue(stack = mutableListOf<Int>(), length = 0), var value: Int = 0)
data class FrontResult(var queue: Queue = Queue(stack = mutableListOf<Int>(), length = 0), var value: Int = 0)
fun empty_queue(): Queue {
    return Queue(stack = mutableListOf<Int>(), length = 0)
}

fun put(q: Queue, item: Int): Queue {
    var s: MutableList<Int> = run { val _tmp = (q.stack).toMutableList(); _tmp.add(item); _tmp }
    return Queue(stack = s, length = q.length + 1)
}

fun drop_first(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i < xs.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun drop_last(xs: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < (xs.size - 1)) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        i = i + 1
    }
    return res
}

fun rotate(q: Queue, rotation: Int): Queue {
    var s: MutableList<Int> = q.stack
    var i: Int = (0).toInt()
    while ((i < rotation) && (s.size > 0)) {
        var temp: Int = (s[0]!!).toInt()
        s = drop_first(s)
        s = run { val _tmp = s.toMutableList(); _tmp.add(temp); _tmp }
        i = i + 1
    }
    return Queue(stack = s, length = q.length)
}

fun get(q: Queue): GetResult {
    if (q.length == 0) {
        panic("queue empty")
    }
    var q1: Queue = rotate(q, 1)
    var v: Int = ((q1.stack)[q1.length - 1]!!).toInt()
    var s: MutableList<Int> = drop_last(q1.stack)
    var q2: Queue = Queue(stack = s, length = q1.length)
    q2 = rotate(q2, q2.length - 1)
    q2 = Queue(stack = q2.stack, length = q2.length - 1)
    return GetResult(queue = q2, value = v)
}

fun front(q: Queue): FrontResult {
    var r: GetResult = get(q)
    var q2: Queue = put(r.queue, r.value)
    q2 = rotate(q2, q2.length - 1)
    return FrontResult(queue = q2, value = r.value)
}

fun size(q: Queue): Int {
    return q.length
}

fun to_string(q: Queue): String {
    var s: String = "<"
    if (q.length > 0) {
        s = s + _numToStr((q.stack)[0]!!)
        var i: Int = (1).toInt()
        while (i < q.length) {
            s = (s + ", ") + _numToStr((q.stack)[i]!!)
            i = i + 1
        }
    }
    s = s + ">"
    return s
}

fun user_main(): Unit {
    var q: Queue = empty_queue()
    q = put(q, 1)
    q = put(q, 2)
    q = put(q, 3)
    println(to_string(q))
    var g: GetResult = get(q)
    q = g.queue
    println(g.value)
    println(to_string(q))
    var f: FrontResult = front(q)
    q = f.queue
    println(f.value)
    println(to_string(q))
    println(size(q))
}

fun main() {
    user_main()
}
