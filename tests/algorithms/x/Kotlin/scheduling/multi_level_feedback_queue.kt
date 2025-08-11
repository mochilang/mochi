fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

data class Process(var process_name: String = "", var arrival_time: Int = 0, var stop_time: Int = 0, var burst_time: Int = 0, var waiting_time: Int = 0, var turnaround_time: Int = 0)
data class MLFQ(var number_of_queues: Int = 0, var time_slices: MutableList<Int> = mutableListOf<Int>(), var ready_queue: MutableList<Process> = mutableListOf<Process>(), var current_time: Int = 0, var finish_queue: MutableList<Process> = mutableListOf<Process>())
data class RRResult(var finished: MutableList<Process> = mutableListOf<Process>(), var ready: MutableList<Process> = mutableListOf<Process>())
var P1: Process = make_process("P1", 0, 53)
var P2: Process = make_process("P2", 0, 17)
var P3: Process = make_process("P3", 0, 68)
var P4: Process = make_process("P4", 0, 24)
var number_of_queues: Int = (3).toInt()
var time_slices: MutableList<Int> = mutableListOf(17, 25)
var queue: MutableList<Process> = mutableListOf(P1, P2, P3, P4)
var mlfq: MLFQ = make_mlfq(number_of_queues, time_slices, queue, 0)
var finish_queue: MutableList<Process> = multi_level_feedback_queue(mlfq)
fun make_process(name: String, arrival: Int, burst: Int): Process {
    return Process(process_name = name, arrival_time = arrival, stop_time = arrival, burst_time = burst, waiting_time = 0, turnaround_time = 0)
}

fun make_mlfq(nqueues: Int, time_slices: MutableList<Int>, queue: MutableList<Process>, current_time: Int): MLFQ {
    return MLFQ(number_of_queues = nqueues, time_slices = time_slices, ready_queue = queue, current_time = current_time, finish_queue = mutableListOf<Process>())
}

fun calculate_sequence_of_finish_queue(mlfq: MLFQ): MutableList<String> {
    var seq: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < (mlfq.finish_queue).size) {
        var p: Process = (mlfq.finish_queue)[i]!!
        seq = run { val _tmp = seq.toMutableList(); _tmp.add(p.process_name); _tmp }
        i = i + 1
    }
    return seq
}

fun calculate_waiting_time(queue: MutableList<Process>): MutableList<Int> {
    var times: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < queue.size) {
        var p: Process = queue[i]!!
        times = run { val _tmp = times.toMutableList(); _tmp.add(p.waiting_time); _tmp }
        i = i + 1
    }
    return times
}

fun calculate_turnaround_time(queue: MutableList<Process>): MutableList<Int> {
    var times: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < queue.size) {
        var p: Process = queue[i]!!
        times = run { val _tmp = times.toMutableList(); _tmp.add(p.turnaround_time); _tmp }
        i = i + 1
    }
    return times
}

fun calculate_completion_time(queue: MutableList<Process>): MutableList<Int> {
    var times: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < queue.size) {
        var p: Process = queue[i]!!
        times = run { val _tmp = times.toMutableList(); _tmp.add(p.stop_time); _tmp }
        i = i + 1
    }
    return times
}

fun calculate_remaining_burst_time_of_processes(queue: MutableList<Process>): MutableList<Int> {
    var times: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < queue.size) {
        var p: Process = queue[i]!!
        times = run { val _tmp = times.toMutableList(); _tmp.add(p.burst_time); _tmp }
        i = i + 1
    }
    return times
}

fun update_waiting_time(mlfq: MLFQ, process: Process): Int {
    process.waiting_time = process.waiting_time + (mlfq.current_time - process.stop_time)
    return process.waiting_time
}

fun first_come_first_served(mlfq: MLFQ, ready_queue: MutableList<Process>): MutableList<Process> {
    var finished: MutableList<Process> = mutableListOf<Process>()
    var rq: MutableList<Process> = ready_queue
    while (rq.size != 0) {
        var cp: Process = rq[0]!!
        rq = _sliceList(rq, 1, rq.size)
        if (mlfq.current_time < cp.arrival_time) {
            mlfq.current_time = cp.arrival_time
        }
        update_waiting_time(mlfq, cp)
        mlfq.current_time = mlfq.current_time + cp.burst_time
        cp.burst_time = 0
        cp.turnaround_time = mlfq.current_time - cp.arrival_time
        cp.stop_time = mlfq.current_time
        finished = run { val _tmp = finished.toMutableList(); _tmp.add(cp); _tmp }
    }
    mlfq.finish_queue = ((concat(mlfq.finish_queue, finished)) as MutableList<Process>)
    return finished
}

fun round_robin(mlfq: MLFQ, ready_queue: MutableList<Process>, time_slice: Int): RRResult {
    var finished: MutableList<Process> = mutableListOf<Process>()
    var rq: MutableList<Process> = ready_queue
    var count: Int = (rq.size).toInt()
    var i: Int = (0).toInt()
    while (i < count) {
        var cp: Process = rq[0]!!
        rq = _sliceList(rq, 1, rq.size)
        if (mlfq.current_time < cp.arrival_time) {
            mlfq.current_time = cp.arrival_time
        }
        update_waiting_time(mlfq, cp)
        if (cp.burst_time > time_slice) {
            mlfq.current_time = mlfq.current_time + time_slice
            cp.burst_time = cp.burst_time - time_slice
            cp.stop_time = mlfq.current_time
            rq = run { val _tmp = rq.toMutableList(); _tmp.add(cp); _tmp }
        } else {
            mlfq.current_time = mlfq.current_time + cp.burst_time
            cp.burst_time = 0
            cp.stop_time = mlfq.current_time
            cp.turnaround_time = mlfq.current_time - cp.arrival_time
            finished = run { val _tmp = finished.toMutableList(); _tmp.add(cp); _tmp }
        }
        i = i + 1
    }
    mlfq.finish_queue = ((concat(mlfq.finish_queue, finished)) as MutableList<Process>)
    return RRResult(finished = finished, ready = rq)
}

fun multi_level_feedback_queue(mlfq: MLFQ): MutableList<Process> {
    var i: Int = (0).toInt()
    while (i < (mlfq.number_of_queues - 1)) {
        var rr: RRResult = round_robin(mlfq, mlfq.ready_queue, (mlfq.time_slices)[i]!!)
        mlfq.ready_queue = rr.ready
        i = i + 1
    }
    first_come_first_served(mlfq, mlfq.ready_queue)
    return mlfq.finish_queue
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("waiting time:\t\t\t" + calculate_waiting_time(mutableListOf(P1, P2, P3, P4)).toString())
        println("completion time:\t\t" + calculate_completion_time(mutableListOf(P1, P2, P3, P4)).toString())
        println("turnaround time:\t\t" + calculate_turnaround_time(mutableListOf(P1, P2, P3, P4)).toString())
        println("sequence of finished processes:\t" + calculate_sequence_of_finish_queue(mlfq).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
