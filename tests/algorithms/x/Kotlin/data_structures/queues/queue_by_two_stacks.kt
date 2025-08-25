import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Queue(var stack1: MutableList<Int> = mutableListOf<Int>(), var stack2: MutableList<Int> = mutableListOf<Int>())
data class GetResult(var queue: Queue = Queue(stack1 = mutableListOf<Int>(), stack2 = mutableListOf<Int>()), var value: Int = 0)
var q: Queue = new_queue(mutableListOf(10, 20, 30))
var r1: GetResult = get(q)
fun new_queue(items: MutableList<Int>): Queue {
    return Queue(stack1 = items, stack2 = mutableListOf<Int>())
}

fun len_queue(q: Queue): Int {
    return (q.stack1).size + (q.stack2).size
}

fun str_queue(q: Queue): String {
    var items: MutableList<Int> = mutableListOf<Int>()
    var i: Int = ((q.stack2).size - 1).toInt()
    while (i >= 0) {
        items = run { val _tmp = items.toMutableList(); _tmp.add((q.stack2)[i]!!); _tmp }
        i = i - 1
    }
    var j: Int = (0).toInt()
    while (j < (q.stack1).size) {
        items = run { val _tmp = items.toMutableList(); _tmp.add((q.stack1)[j]!!); _tmp }
        j = j + 1
    }
    var s: String = "Queue(("
    var k: Int = (0).toInt()
    while (k < items.size) {
        s = s + _numToStr(items[k]!!)
        if (k < (items.size - 1)) {
            s = s + ", "
        }
        k = k + 1
    }
    s = s + "))"
    return s
}

fun put(q: Queue, item: Int): Queue {
    var s1: MutableList<Int> = q.stack1
    s1 = run { val _tmp = s1.toMutableList(); _tmp.add(item); _tmp }
    return Queue(stack1 = s1, stack2 = q.stack2)
}

fun get(q: Queue): GetResult {
    var s1: MutableList<Int> = q.stack1
    var s2: MutableList<Int> = q.stack2
    if (s2.size == 0) {
        while (s1.size > 0) {
            var idx: Int = (s1.size - 1).toInt()
            var v: Int = (s1[idx]!!).toInt()
            var new_s1: MutableList<Int> = mutableListOf<Int>()
            var i: Int = (0).toInt()
            while (i < idx) {
                new_s1 = run { val _tmp = new_s1.toMutableList(); _tmp.add(s1[i]!!); _tmp }
                i = i + 1
            }
            s1 = new_s1
            s2 = run { val _tmp = s2.toMutableList(); _tmp.add(v); _tmp }
        }
    }
    if (s2.size == 0) {
        panic("Queue is empty")
    }
    var idx2: Int = (s2.size - 1).toInt()
    var value: Int = (s2[idx2]!!).toInt()
    var new_s2: MutableList<Int> = mutableListOf<Int>()
    var j: Int = (0).toInt()
    while (j < idx2) {
        new_s2 = run { val _tmp = new_s2.toMutableList(); _tmp.add(s2[j]!!); _tmp }
        j = j + 1
    }
    s2 = new_s2
    return GetResult(queue = Queue(stack1 = s1, stack2 = s2), value = value)
}

fun main() {
    q = r1.queue
    println(r1.value)
    q = put(q, 40)
    var r2: GetResult = get(q)
    q = r2.queue
    println(r2.value)
    var r3: GetResult = get(q)
    q = r3.queue
    println(r3.value)
    println(len_queue(q))
    var r4: GetResult = get(q)
    q = r4.queue
    println(r4.value)
}
