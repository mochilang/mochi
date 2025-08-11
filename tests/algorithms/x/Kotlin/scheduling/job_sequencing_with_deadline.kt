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

data class Job(var id: Int = 0, var deadline: Int = 0, var profit: Int = 0)
var jobs1: MutableList<Job> = mutableListOf<Job>()
fun sort_jobs_by_profit(jobs: MutableList<Job>): MutableList<Job> {
    var js: MutableList<Job> = jobs
    var i: Int = (0).toInt()
    while (i < js.size) {
        var j: Int = (0).toInt()
        while (j < ((js.size - i) - 1)) {
            var a: Job = js[j]!!
            var b: Job = js[j + 1]!!
            if (a.profit < b.profit) {
                _listSet(js, j, b)
                _listSet(js, j + 1, a)
            }
            j = j + 1
        }
        i = i + 1
    }
    return js
}

fun max_deadline(jobs: MutableList<Job>): Int {
    var max_d: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < jobs.size) {
        var job: Job = jobs[i]!!
        var d: Int = (job.deadline).toInt()
        if (d > max_d) {
            max_d = d
        }
        i = i + 1
    }
    return max_d
}

fun job_sequencing_with_deadlines(jobs: MutableList<Job>): MutableList<Int> {
    var js: MutableList<Job> = sort_jobs_by_profit(jobs)
    var max_d: Int = (max_deadline(js)).toInt()
    var time_slots: MutableList<Int> = mutableListOf<Int>()
    var t: Int = (0).toInt()
    while (t < max_d) {
        time_slots = run { val _tmp = time_slots.toMutableList(); _tmp.add(0 - 1); _tmp }
        t = t + 1
    }
    var count: Int = (0).toInt()
    var max_profit: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < js.size) {
        var job: Job = js[i]!!
        var j: BigInteger = ((job.deadline - 1).toBigInteger())
        while (j.compareTo((0).toBigInteger()) >= 0) {
            if (time_slots[(j).toInt()]!! == (0 - 1)) {
                _listSet(time_slots, (j).toInt(), job.id)
                count = count + 1
                max_profit = max_profit + job.profit
                break
            }
            j = j.subtract((1).toBigInteger())
        }
        i = i + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    result = run { val _tmp = result.toMutableList(); _tmp.add(count); _tmp }
    result = run { val _tmp = result.toMutableList(); _tmp.add(max_profit); _tmp }
    return result
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        jobs1 = run { val _tmp = jobs1.toMutableList(); _tmp.add(Job(id = 1, deadline = 4, profit = 20)); _tmp }
        jobs1 = run { val _tmp = jobs1.toMutableList(); _tmp.add(Job(id = 2, deadline = 1, profit = 10)); _tmp }
        jobs1 = run { val _tmp = jobs1.toMutableList(); _tmp.add(Job(id = 3, deadline = 1, profit = 40)); _tmp }
        jobs1 = run { val _tmp = jobs1.toMutableList(); _tmp.add(Job(id = 4, deadline = 1, profit = 30)); _tmp }
        println(job_sequencing_with_deadlines(jobs1).toString())
        var jobs2: MutableList<Job> = mutableListOf<Job>()
        jobs2 = run { val _tmp = jobs2.toMutableList(); _tmp.add(Job(id = 1, deadline = 2, profit = 100)); _tmp }
        jobs2 = run { val _tmp = jobs2.toMutableList(); _tmp.add(Job(id = 2, deadline = 1, profit = 19)); _tmp }
        jobs2 = run { val _tmp = jobs2.toMutableList(); _tmp.add(Job(id = 3, deadline = 2, profit = 27)); _tmp }
        jobs2 = run { val _tmp = jobs2.toMutableList(); _tmp.add(Job(id = 4, deadline = 1, profit = 25)); _tmp }
        jobs2 = run { val _tmp = jobs2.toMutableList(); _tmp.add(Job(id = 5, deadline = 1, profit = 15)); _tmp }
        println(job_sequencing_with_deadlines(jobs2).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
