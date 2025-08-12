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

fun index_of(xs: MutableList<Int>, x: Int): Int {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun majority_vote(votes: MutableList<Int>, votes_needed_to_win: Int): MutableList<Int> {
    if (votes_needed_to_win < 2) {
        return mutableListOf<Int>()
    }
    var candidates: MutableList<Int> = mutableListOf<Int>()
    var counts: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < votes.size) {
        var v: Int = (votes[i]!!).toInt()
        var idx: Int = (index_of(candidates, v)).toInt()
        if (idx != (0 - 1)) {
            _listSet(counts, idx, counts[idx]!! + 1)
        } else {
            if (candidates.size < (votes_needed_to_win - 1)) {
                candidates = run { val _tmp = candidates.toMutableList(); _tmp.add(v); _tmp }
                counts = run { val _tmp = counts.toMutableList(); _tmp.add(1); _tmp }
            } else {
                var j: Int = (0).toInt()
                while (j < counts.size) {
                    _listSet(counts, j, counts[j]!! - 1)
                    j = j + 1
                }
                var new_candidates: MutableList<Int> = mutableListOf<Int>()
                var new_counts: MutableList<Int> = mutableListOf<Int>()
                j = 0
                while (j < candidates.size) {
                    if (counts[j]!! > 0) {
                        new_candidates = run { val _tmp = new_candidates.toMutableList(); _tmp.add(candidates[j]!!); _tmp }
                        new_counts = run { val _tmp = new_counts.toMutableList(); _tmp.add(counts[j]!!); _tmp }
                    }
                    j = j + 1
                }
                candidates = new_candidates
                counts = new_counts
            }
        }
        i = i + 1
    }
    var final_counts: MutableList<Int> = mutableListOf<Int>()
    var j: Int = (0).toInt()
    while (j < candidates.size) {
        final_counts = run { val _tmp = final_counts.toMutableList(); _tmp.add(0); _tmp }
        j = j + 1
    }
    i = 0
    while (i < votes.size) {
        var v: Int = (votes[i]!!).toInt()
        var idx: Int = (index_of(candidates, v)).toInt()
        if (idx != (0 - 1)) {
            _listSet(final_counts, idx, final_counts[idx]!! + 1)
        }
        i = i + 1
    }
    var result: MutableList<Int> = mutableListOf<Int>()
    j = 0
    while (j < candidates.size) {
        if ((final_counts[j]!! * votes_needed_to_win) > votes.size) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(candidates[j]!!); _tmp }
        }
        j = j + 1
    }
    return result
}

fun user_main(): Unit {
    var votes: MutableList<Int> = mutableListOf(1, 2, 2, 3, 1, 3, 2)
    println(majority_vote(votes, 3).toString())
    println(majority_vote(votes, 2).toString())
    println(majority_vote(votes, 4).toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
