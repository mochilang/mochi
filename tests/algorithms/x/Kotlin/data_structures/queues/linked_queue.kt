import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class Node(var data: String = "", var next: Int = 0)
data class LinkedQueue(var nodes: MutableList<Node> = mutableListOf<Node>(), var front: Int = 0, var rear: Int = 0)
var queue: LinkedQueue = new_queue()
fun new_queue(): LinkedQueue {
    return LinkedQueue(nodes = mutableListOf<Node>(), front = 0 - 1, rear = 0 - 1)
}

fun is_empty(q: LinkedQueue): Boolean {
    return q.front == (0 - 1)
}

fun put(q: LinkedQueue, item: String): Unit {
    var node: Node = Node(data = item, next = 0 - 1)
    q.nodes = run { val _tmp = (q.nodes).toMutableList(); _tmp.add(node); _tmp }
    var idx: Int = ((q.nodes).size - 1).toInt()
    if (q.front == (0 - 1)) {
        q.front = idx
        q.rear = idx
    } else {
        var nodes: MutableList<Node> = q.nodes
        nodes[q.rear]!!.next = idx
        q.nodes = nodes
        q.rear = idx
    }
}

fun get(q: LinkedQueue): String {
    if ((is_empty(q)) as Boolean) {
        panic("dequeue from empty queue")
    }
    var idx: Int = (q.front).toInt()
    var node: Node = (q.nodes)[idx]!!
    q.front = node.next
    if (q.front == (0 - 1)) {
        q.rear = 0 - 1
    }
    return node.data
}

fun length(q: LinkedQueue): Int {
    var count: Int = (0).toInt()
    var idx: Int = (q.front).toInt()
    while (idx != (0 - 1)) {
        count = count + 1
        idx = (q.nodes)[idx]!!.next
    }
    return count
}

fun to_string(q: LinkedQueue): String {
    var res: String = ""
    var idx: Int = (q.front).toInt()
    var first: Boolean = true
    while (idx != (0 - 1)) {
        var node: Node = (q.nodes)[idx]!!
        if (first as Boolean) {
            res = node.data
            first = false
        } else {
            res = (res + " <- ") + node.data
        }
        idx = node.next
    }
    return res
}

fun clear(q: LinkedQueue): Unit {
    q.nodes = mutableListOf<Node>()
    q.front = 0 - 1
    q.rear = 0 - 1
}

fun main() {
    println(is_empty(queue).toString())
    put(queue, "5")
    put(queue, "9")
    put(queue, "python")
    println(is_empty(queue).toString())
    println(get(queue))
    put(queue, "algorithms")
    println(get(queue))
    println(get(queue))
    println(get(queue))
    println(is_empty(queue).toString())
}
