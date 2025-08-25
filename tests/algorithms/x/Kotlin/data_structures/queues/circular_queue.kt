val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class CircularQueue(var data: MutableList<Int> = mutableListOf<Int>(), var front: Int = 0, var rear: Int = 0, var size: Int = 0, var capacity: Int = 0)
data class DequeueResult(var queue: CircularQueue = CircularQueue(data = mutableListOf<Int>(), front = 0, rear = 0, size = 0, capacity = 0), var value: Int = 0)
fun create_queue(capacity: Int): CircularQueue {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < capacity) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    return CircularQueue(data = arr, front = 0, rear = 0, size = 0, capacity = capacity)
}

fun length(q: CircularQueue): Int {
    return q.size
}

fun is_empty(q: CircularQueue): Boolean {
    return q.size == 0
}

fun front(q: CircularQueue): Int {
    if ((is_empty(q)) as Boolean) {
        return 0
    }
    return (q.data)[q.front]!!
}

fun enqueue(q: CircularQueue, value: Int): CircularQueue {
    if (q.size >= q.capacity) {
        panic("QUEUE IS FULL")
    }
    var arr: MutableList<Int> = q.data
    _listSet(arr, q.rear, value)
    q.data = arr
    q.rear = Math.floorMod((q.rear + 1), q.capacity)
    q.size = q.size + 1
    return q
}

fun dequeue(q: CircularQueue): DequeueResult {
    if (q.size == 0) {
        panic("UNDERFLOW")
    }
    var value: Int = ((q.data)[q.front]!!).toInt()
    var arr2: MutableList<Int> = q.data
    _listSet(arr2, q.front, 0)
    q.data = arr2
    q.front = Math.floorMod((q.front + 1), q.capacity)
    q.size = q.size - 1
    return DequeueResult(queue = q, value = value)
}

fun user_main(): Unit {
    var q: CircularQueue = create_queue(5)
    println(is_empty(q))
    q = enqueue(q, 10)
    println(is_empty(q))
    q = enqueue(q, 20)
    q = enqueue(q, 30)
    println(front(q))
    var r: DequeueResult = dequeue(q)
    q = r.queue
    println(r.value)
    println(front(q))
    println(length(q))
}

fun main() {
    user_main()
}
