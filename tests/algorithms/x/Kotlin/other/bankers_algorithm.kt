fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
}

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

data class State(var claim: MutableList<Int> = mutableListOf<Int>(), var alloc: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>(), var max: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>())
var claim_vector: MutableList<Int> = mutableListOf(8, 5, 9, 7)
var allocated_resources_table: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(2, 0, 1, 1), mutableListOf(0, 1, 2, 1), mutableListOf(4, 0, 0, 3), mutableListOf(0, 2, 1, 0), mutableListOf(1, 0, 3, 0))
var maximum_claim_table: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(3, 2, 1, 4), mutableListOf(0, 2, 5, 2), mutableListOf(5, 1, 0, 5), mutableListOf(1, 5, 3, 0), mutableListOf(3, 0, 3, 3))
fun processes_resource_summation(alloc: MutableList<MutableList<Int>>): MutableList<Int> {
    var resources: Int = ((alloc[0]!!).size).toInt()
    var sums: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < resources) {
        var total: Int = (0).toInt()
        var j: Int = (0).toInt()
        while (j < alloc.size) {
            total = total + (((alloc[j]!!) as MutableList<Int>))[i]!!
            j = j + 1
        }
        sums = run { val _tmp = sums.toMutableList(); _tmp.add(total); _tmp }
        i = i + 1
    }
    return sums
}

fun available_resources(claim: MutableList<Int>, alloc_sum: MutableList<Int>): MutableList<Int> {
    var avail: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < claim.size) {
        avail = run { val _tmp = avail.toMutableList(); _tmp.add(claim[i]!! - alloc_sum[i]!!); _tmp }
        i = i + 1
    }
    return avail
}

fun need(max: MutableList<MutableList<Int>>, alloc: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var needs: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < max.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < (max[0]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((max[i]!!) as MutableList<Int>))[j]!! - (((alloc[i]!!) as MutableList<Int>))[j]!!); _tmp }
            j = j + 1
        }
        needs = run { val _tmp = needs.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return needs
}

fun pretty_print(claim: MutableList<Int>, alloc: MutableList<MutableList<Int>>, max: MutableList<MutableList<Int>>): Unit {
    println("         Allocated Resource Table")
    var i: Int = (0).toInt()
    while (i < alloc.size) {
        var row: MutableList<Int> = alloc[i]!!
        var line: String = ("P" + (i + 1).toString()) + "       "
        var j: Int = (0).toInt()
        while (j < row.size) {
            line = line + (row[j]!!).toString()
            if (j < (row.size - 1)) {
                line = line + "        "
            }
            j = j + 1
        }
        println(line)
        println("")
        i = i + 1
    }
    println("         System Resource Table")
    i = 0
    while (i < max.size) {
        var row: MutableList<Int> = max[i]!!
        var line: String = ("P" + (i + 1).toString()) + "       "
        var j: Int = (0).toInt()
        while (j < _len(row)) {
            line = line + ((row as MutableList<Any?>)[j]).toString()
            if (j < (_len(row) - 1)) {
                line = line + "        "
            }
            j = j + 1
        }
        println(line)
        println("")
        i = i + 1
    }
    var usage: String = ""
    i = 0
    while (i < claim.size) {
        if (i > 0) {
            usage = usage + " "
        }
        usage = usage + (claim[i]!!).toString()
        i = i + 1
    }
    var alloc_sum: MutableList<Int> = processes_resource_summation(alloc)
    var avail: MutableList<Int> = available_resources(claim, alloc_sum)
    var avail_str: String = ""
    i = 0
    while (i < avail.size) {
        if (i > 0) {
            avail_str = avail_str + " "
        }
        avail_str = avail_str + (avail[i]!!).toString()
        i = i + 1
    }
    println("Current Usage by Active Processes: " + usage)
    println("Initial Available Resources:       " + avail_str)
}

fun bankers_algorithm(claim: MutableList<Int>, alloc: MutableList<MutableList<Int>>, max: MutableList<MutableList<Int>>): Unit {
    var need_list: MutableList<MutableList<Int>> = need(max, alloc)
    var alloc_sum: MutableList<Int> = processes_resource_summation(alloc)
    var avail: MutableList<Int> = available_resources(claim, alloc_sum)
    println("__________________________________________________")
    println("")
    var finished: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i < _len(need_list)) {
        finished = run { val _tmp = finished.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    var remaining: Int = (_len(need_list)).toInt()
    while (remaining > 0) {
        var safe: Boolean = false
        var p: Int = (0).toInt()
        while (p < _len(need_list)) {
            if (!((finished[p]!!) as? Boolean ?: false)) {
                var exec: Boolean = true
                var r: Int = (0).toInt()
                while (r < avail.size) {
                    if (((need_list as MutableList<Any?>)[p] as MutableList<Int>)[r] > avail[r]!!) {
                        exec = false
                        break
                    }
                    r = r + 1
                }
                if ((exec as Boolean)) {
                    safe = true
                    println(("Process " + (p + 1).toString()) + " is executing.")
                    r = 0
                    while (r < avail.size) {
                        _listSet(avail, r, avail[r]!! + (((alloc[p]!!) as MutableList<Int>))[r]!!)
                        r = r + 1
                    }
                    var avail_str: String = ""
                    r = 0
                    while (r < avail.size) {
                        if (r > 0) {
                            avail_str = avail_str + " "
                        }
                        avail_str = avail_str + (avail[r]!!).toString()
                        r = r + 1
                    }
                    println("Updated available resource stack for processes: " + avail_str)
                    println("The process is in a safe state.")
                    println("")
                    _listSet(finished, p, true)
                    remaining = remaining - 1
                }
            }
            p = p + 1
        }
        if (!safe) {
            println("System in unsafe state. Aborting...")
            println("")
            break
        }
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        pretty_print(claim_vector, allocated_resources_table, maximum_claim_table)
        bankers_algorithm(claim_vector, allocated_resources_table, maximum_claim_table)
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
