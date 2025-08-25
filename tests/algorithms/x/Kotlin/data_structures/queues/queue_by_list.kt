val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Queue(var entries: MutableList<Int> = mutableListOf<Int>())
data class GetResult(var queue: Queue = Queue(entries = mutableListOf<Int>()), var value: Int = 0)
var q: Queue = new_queue(mutableListOf<Int>())
fun new_queue(items: MutableList<Int>): Queue {
    return Queue(entries = items)
}

fun len_queue(q: Queue): Int {
    return (q.entries).size
}

fun str_queue(q: Queue): String {
    var s: String = "Queue(("
    var i: Int = (0).toInt()
    while (i < (q.entries).size) {
        s = s + _numToStr((q.entries)[i]!!)
        if (i < ((q.entries).size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "))"
    return s
}

fun put(q: Queue, item: Int): Queue {
    var e: MutableList<Int> = q.entries
    e = run { val _tmp = e.toMutableList(); _tmp.add(item); _tmp }
    return Queue(entries = e)
}

fun get(q: Queue): GetResult {
    if ((q.entries).size == 0) {
        panic("Queue is empty")
    }
    var value: Int = ((q.entries)[0]!!).toInt()
    var new_entries: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (1).toInt()
    while (i < (q.entries).size) {
        new_entries = run { val _tmp = new_entries.toMutableList(); _tmp.add((q.entries)[i]!!); _tmp }
        i = i + 1
    }
    return GetResult(queue = Queue(entries = new_entries), value = value)
}

fun rotate(q: Queue, rotation: Int): Queue {
    var e: MutableList<Int> = q.entries
    var r: Int = (0).toInt()
    while (r < rotation) {
        if (e.size > 0) {
            var first: Int = (e[0]!!).toInt()
            var rest: MutableList<Int> = mutableListOf<Int>()
            var i: Int = (1).toInt()
            while (i < e.size) {
                rest = run { val _tmp = rest.toMutableList(); _tmp.add(e[i]!!); _tmp }
                i = i + 1
            }
            rest = run { val _tmp = rest.toMutableList(); _tmp.add(first); _tmp }
            e = rest
        }
        r = r + 1
    }
    return Queue(entries = e)
}

fun get_front(q: Queue): Int {
    return (q.entries)[0]!!
}

fun main() {
    println(len_queue(q))
    q = put(q, 10)
    q = put(q, 20)
    q = put(q, 30)
    q = put(q, 40)
    println(str_queue(q))
    var res: GetResult = get(q)
    q = res.queue
    println(res.value)
    println(str_queue(q))
    q = rotate(q, 2)
    println(str_queue(q))
    var front: Int = (get_front(q)).toInt()
    println(front)
    println(str_queue(q))
}
