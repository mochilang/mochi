import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

fun calculate_waitingtime(arrival_time: MutableList<Int>, burst_time: MutableList<Int>, no_of_processes: Int): MutableList<Int> {
    var waiting_time: MutableList<Int> = mutableListOf<Int>()
    var remaining_time: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < no_of_processes) {
        waiting_time = run { val _tmp = waiting_time.toMutableList(); _tmp.add(0); _tmp }
        remaining_time = run { val _tmp = remaining_time.toMutableList(); _tmp.add(burst_time[i]!!); _tmp }
        i = (i + 1).toInt()
    }
    var completed: Int = (0).toInt()
    var total_time: Int = (0).toInt()
    while (completed != no_of_processes) {
        var ready_process: MutableList<Int> = mutableListOf<Int>()
        var target_process: Int = (0 - 1).toInt()
        var j: Int = (0).toInt()
        while (j < no_of_processes) {
            if ((arrival_time[j]!! <= total_time) && (remaining_time[j]!! > 0)) {
                ready_process = run { val _tmp = ready_process.toMutableList(); _tmp.add(j); _tmp }
            }
            j = j + 1
        }
        if (ready_process.size > 0) {
            target_process = ready_process[0]!!
            var k: Int = (0).toInt()
            while (k < ready_process.size) {
                var idx: Int = (ready_process[k]!!).toInt()
                if (remaining_time[idx]!! < remaining_time[target_process]!!) {
                    target_process = idx
                }
                k = k + 1
            }
            total_time = total_time + burst_time[target_process]!!
            completed = completed + 1
            _listSet(remaining_time, target_process, 0)
            _listSet(waiting_time, target_process, (total_time - arrival_time[target_process]!!) - burst_time[target_process]!!)
        } else {
            total_time = total_time + 1
        }
    }
    return waiting_time
}

fun calculate_turnaroundtime(burst_time: MutableList<Int>, no_of_processes: Int, waiting_time: MutableList<Int>): MutableList<Int> {
    var turn_around_time: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < no_of_processes) {
        turn_around_time = run { val _tmp = turn_around_time.toMutableList(); _tmp.add(burst_time[i]!! + waiting_time[i]!!); _tmp }
        i = (i + 1).toInt()
    }
    return turn_around_time
}

fun average(values: MutableList<Int>): Double {
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < values.size) {
        total = total + values[i]!!
        i = (i + 1).toInt()
    }
    return ((total.toDouble())) / ((values.size.toDouble()))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("[TEST CASE 01]")
        var no_of_processes: Int = (4).toInt()
        var burst_time: MutableList<Int> = mutableListOf(2, 5, 3, 7)
        var arrival_time: MutableList<Int> = mutableListOf(0, 0, 0, 0)
        var waiting_time: MutableList<Int> = calculate_waitingtime(arrival_time, burst_time, no_of_processes)
        var turn_around_time: MutableList<Int> = calculate_turnaroundtime(burst_time, no_of_processes, waiting_time)
        println("PID\tBurst Time\tArrival Time\tWaiting Time\tTurnaround Time")
        var i: Int = (0).toInt()
        while (i < no_of_processes) {
            var pid: Int = (i + 1).toInt()
            println((((((((pid.toString() + "\t") + (burst_time[i]!!).toString()) + "\t\t\t") + (arrival_time[i]!!).toString()) + "\t\t\t\t") + (waiting_time[i]!!).toString()) + "\t\t\t\t") + (turn_around_time[i]!!).toString())
            i = (i + 1).toInt()
        }
        var avg_wait: Double = average(waiting_time)
        var avg_turn: Double = average(turn_around_time)
        println("\nAverage waiting time = " + avg_wait.toString())
        println("Average turnaround time = " + avg_turn.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
