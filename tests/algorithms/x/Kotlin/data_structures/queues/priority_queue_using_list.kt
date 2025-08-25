val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/queues"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class FixedPriorityQueue(var queues: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>())
data class FPQDequeueResult(var queue: FixedPriorityQueue = FixedPriorityQueue(queues = mutableListOf<MutableList<Int>>()), var value: Int = 0)
data class ElementPriorityQueue(var queue: MutableList<Int> = mutableListOf<Int>())
data class EPQDequeueResult(var queue: ElementPriorityQueue = ElementPriorityQueue(queue = mutableListOf<Int>()), var value: Int = 0)
fun panic(msg: String): Nothing {
    println(msg)
    throw RuntimeException(msg)
}

fun fpq_new(): FixedPriorityQueue {
    return FixedPriorityQueue(queues = mutableListOf<Any?>(mutableListOf<Any?>() as Any?, mutableListOf<Any?>() as Any?, mutableListOf<Any?>() as Any?))
}

fun fpq_enqueue(fpq: FixedPriorityQueue, priority: Int, data: Int): FixedPriorityQueue {
    if ((priority < 0) || (priority >= (fpq.queues).size)) {
        panic("Valid priorities are 0, 1, and 2")
        return fpq
    }
    if (((fpq.queues)[priority]!!).size >= 100) {
        panic("Maximum queue size is 100")
        return fpq
    }
    var qs: MutableList<MutableList<Int>> = fpq.queues
    _listSet(qs, priority, run { val _tmp = (qs[priority]!!).toMutableList(); _tmp.add(data); _tmp })
    fpq.queues = qs
    return fpq
}

fun fpq_dequeue(fpq: FixedPriorityQueue): FPQDequeueResult {
    var qs: MutableList<MutableList<Int>> = fpq.queues
    var i: Int = (0).toInt()
    while (i < qs.size) {
        var q: MutableList<Int> = qs[i]!!
        if (q.size > 0) {
            var _val: Int = (q[0]!!).toInt()
            var new_q: MutableList<Int> = mutableListOf<Int>()
            var j: Int = (1).toInt()
            while (j < q.size) {
                new_q = run { val _tmp = new_q.toMutableList(); _tmp.add(q[j]!!); _tmp }
                j = j + 1
            }
            _listSet(qs, i, new_q)
            fpq.queues = qs
            return FPQDequeueResult(queue = fpq, value = _val)
        }
        i = i + 1
    }
    panic("All queues are empty")
    return FPQDequeueResult(queue = fpq, value = 0)
}

fun fpq_to_string(fpq: FixedPriorityQueue): String {
    var lines: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < (fpq.queues).size) {
        var q_str: String = "["
        var q: MutableList<Int> = (fpq.queues)[i]!!
        var j: Int = (0).toInt()
        while (j < q.size) {
            if (j > 0) {
                q_str = q_str + ", "
            }
            q_str = q_str + _numToStr(q[j]!!)
            j = j + 1
        }
        q_str = q_str + "]"
        lines = run { val _tmp = lines.toMutableList(); _tmp.add((("Priority " + _numToStr(i)) + ": ") + q_str); _tmp }
        i = i + 1
    }
    var res: String = ""
    i = 0
    while (i < lines.size) {
        if (i > 0) {
            res = res + "\n"
        }
        res = res + lines[i]!!
        i = i + 1
    }
    return res
}

fun epq_new(): ElementPriorityQueue {
    return ElementPriorityQueue(queue = mutableListOf<Int>())
}

fun epq_enqueue(epq: ElementPriorityQueue, data: Int): ElementPriorityQueue {
    if ((epq.queue).size >= 100) {
        panic("Maximum queue size is 100")
        return epq
    }
    epq.queue = run { val _tmp = (epq.queue).toMutableList(); _tmp.add(data); _tmp }
    return epq
}

fun epq_dequeue(epq: ElementPriorityQueue): EPQDequeueResult {
    if ((epq.queue).size == 0) {
        panic("The queue is empty")
        return EPQDequeueResult(queue = epq, value = 0)
    }
    var min_val: Int = ((epq.queue)[0]!!).toInt()
    var idx: Int = (0).toInt()
    var i: Int = (1).toInt()
    while (i < (epq.queue).size) {
        var v: Int = ((epq.queue)[i]!!).toInt()
        if (v < min_val) {
            min_val = v
            idx = i
        }
        i = i + 1
    }
    var new_q: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < (epq.queue).size) {
        if (i != idx) {
            new_q = run { val _tmp = new_q.toMutableList(); _tmp.add((epq.queue)[i]!!); _tmp }
        }
        i = i + 1
    }
    epq.queue = new_q
    return EPQDequeueResult(queue = epq, value = min_val)
}

fun epq_to_string(epq: ElementPriorityQueue): String {
    return epq.queue.toString()
}

fun fixed_priority_queue(): Unit {
    var fpq: FixedPriorityQueue = fpq_new()
    fpq = fpq_enqueue(fpq, 0, 10)
    fpq = fpq_enqueue(fpq, 1, 70)
    fpq = fpq_enqueue(fpq, 0, 100)
    fpq = fpq_enqueue(fpq, 2, 1)
    fpq = fpq_enqueue(fpq, 2, 5)
    fpq = fpq_enqueue(fpq, 1, 7)
    fpq = fpq_enqueue(fpq, 2, 4)
    fpq = fpq_enqueue(fpq, 1, 64)
    fpq = fpq_enqueue(fpq, 0, 128)
    println(fpq_to_string(fpq))
    var res: FPQDequeueResult = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    println(fpq_to_string(fpq))
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
    res = fpq_dequeue(fpq)
    fpq = res.queue
    println(res.value)
}

fun element_priority_queue(): Unit {
    var epq: ElementPriorityQueue = epq_new()
    epq = epq_enqueue(epq, 10)
    epq = epq_enqueue(epq, 70)
    epq = epq_enqueue(epq, 100)
    epq = epq_enqueue(epq, 1)
    epq = epq_enqueue(epq, 5)
    epq = epq_enqueue(epq, 7)
    epq = epq_enqueue(epq, 4)
    epq = epq_enqueue(epq, 64)
    epq = epq_enqueue(epq, 128)
    println(epq_to_string(epq))
    var res: EPQDequeueResult = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    println(epq_to_string(epq))
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
    res = epq_dequeue(epq)
    epq = res.queue
    println(res.value)
}

fun user_main(): Unit {
    fixed_priority_queue()
    element_priority_queue()
}

fun main() {
    user_main()
}
