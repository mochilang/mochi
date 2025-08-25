val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class CircularQueue(var data: MutableList<String> = mutableListOf<String>(), var next: MutableList<Int> = mutableListOf<Int>(), var prev: MutableList<Int> = mutableListOf<Int>(), var front: Int = 0, var rear: Int = 0)
data class DequeueResult(var queue: CircularQueue = CircularQueue(data = mutableListOf<String>(), next = mutableListOf<Int>(), prev = mutableListOf<Int>(), front = 0, rear = 0), var value: String = "")
fun create_queue(capacity: Int): CircularQueue {
    var data: MutableList<String> = mutableListOf<String>()
    var next: MutableList<Int> = mutableListOf<Int>()
    var prev: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < capacity) {
        data = run { val _tmp = data.toMutableList(); _tmp.add(""); _tmp }
        next = run { val _tmp = next.toMutableList(); _tmp.add(Math.floorMod((i + 1), capacity)); _tmp }
        prev = run { val _tmp = prev.toMutableList(); _tmp.add(Math.floorMod(((i - 1) + capacity), capacity)); _tmp }
        i = i + 1
    }
    return CircularQueue(data = data, next = next, prev = prev, front = 0, rear = 0)
}

fun is_empty(q: CircularQueue): Boolean {
    return ((q.front == q.rear) && ((q.data)[q.front]!! == "")) as Boolean
}

fun check_can_perform(q: CircularQueue): Unit {
    if ((is_empty(q)) as Boolean) {
        panic("Empty Queue")
    }
}

fun check_is_full(q: CircularQueue): Unit {
    if ((q.next)[q.rear]!! == q.front) {
        panic("Full Queue")
    }
}

fun peek(q: CircularQueue): String {
    check_can_perform(q)
    return (q.data)[q.front]!!
}

fun enqueue(q: CircularQueue, value: String): CircularQueue {
    check_is_full(q)
    if (!is_empty(q)) {
        q.rear = (q.next)[q.rear]!!
    }
    var data: MutableList<String> = q.data
    _listSet(data, q.rear, value)
    q.data = data
    return q
}

fun dequeue(q: CircularQueue): DequeueResult {
    check_can_perform(q)
    var data: MutableList<String> = q.data
    var _val: String = data[q.front]!!
    _listSet(data, q.front, "")
    q.data = data
    if (q.front != q.rear) {
        q.front = (q.next)[q.front]!!
    }
    return DequeueResult(queue = q, value = _val)
}

fun user_main(): Unit {
    var q: CircularQueue = create_queue(3)
    println(is_empty(q).toString())
    q = enqueue(q, "a")
    q = enqueue(q, "b")
    println(peek(q))
    var res: DequeueResult = dequeue(q)
    q = res.queue
    println(res.value)
    res = dequeue(q)
    q = res.queue
    println(res.value)
    println(is_empty(q).toString())
}

fun main() {
    user_main()
}
