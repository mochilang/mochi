val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/dynamic_programming"

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

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

var observations: MutableList<String> = mutableListOf("normal", "cold", "dizzy")
var states: MutableList<String> = mutableListOf("Healthy", "Fever")
var start_p: MutableMap<String, Double> = mutableMapOf<String, Double>("Healthy" to (0.6), "Fever" to (0.4)) as MutableMap<String, Double>
var trans_p: MutableMap<String, MutableMap<String, Double>> = mutableMapOf<String, MutableMap<String, Double>>("Healthy" to (mutableMapOf<String, Double>("Healthy" to (0.7), "Fever" to (0.3))), "Fever" to (mutableMapOf<String, Double>("Healthy" to (0.4), "Fever" to (0.6)))) as MutableMap<String, MutableMap<String, Double>>
var emit_p: MutableMap<String, MutableMap<String, Double>> = mutableMapOf<String, MutableMap<String, Double>>("Healthy" to (mutableMapOf<String, Double>("normal" to (0.5), "cold" to (0.4), "dizzy" to (0.1))), "Fever" to (mutableMapOf<String, Double>("normal" to (0.1), "cold" to (0.3), "dizzy" to (0.6)))) as MutableMap<String, MutableMap<String, Double>>
var result: MutableList<String> = viterbi(observations, states, start_p, trans_p, emit_p)
fun key(state: String, obs: String): String {
    return (state + "|") + obs
}

fun viterbi(observations: MutableList<String>, states: MutableList<String>, start_p: MutableMap<String, Double>, trans_p: MutableMap<String, MutableMap<String, Double>>, emit_p: MutableMap<String, MutableMap<String, Double>>): MutableList<String> {
    if ((observations.size == 0) || (states.size == 0)) {
        panic("empty parameters")
    }
    var probs: MutableMap<String, Double> = mutableMapOf<String, Double>()
    var ptrs: MutableMap<String, String> = mutableMapOf<String, String>()
    var first_obs: String = observations[0]!!
    var i: Int = (0).toInt()
    while (i < states.size) {
        var state: String = states[i]!!
        (probs)[key(state, first_obs)] = (start_p)[state] as Double * (((emit_p)[state] as MutableMap<String, Double>) as MutableMap<String, Double>)[first_obs] as Double
        (ptrs)[key(state, first_obs)] = ""
        i = i + 1
    }
    var t: Int = (1).toInt()
    while (t < observations.size) {
        var obs: String = observations[t]!!
        var j: Int = (0).toInt()
        while (j < states.size) {
            var state: String = states[j]!!
            var max_prob: Double = 0.0 - 1.0
            var prev_state: String = ""
            var k: Int = (0).toInt()
            while (k < states.size) {
                var state0: String = states[k]!!
                var obs0: String = observations[t - 1]!!
                var prob_prev: Double = (probs)[key(state0, obs0)] as Double
                var prob: Double = (prob_prev * (((trans_p)[state0] as MutableMap<String, Double>) as MutableMap<String, Double>)[state] as Double) * (((emit_p)[state] as MutableMap<String, Double>) as MutableMap<String, Double>)[obs] as Double
                if (prob > max_prob) {
                    max_prob = prob
                    prev_state = state0
                }
                k = k + 1
            }
            (probs)[key(state, obs)] = max_prob
            (ptrs)[key(state, obs)] = prev_state
            j = j + 1
        }
        t = t + 1
    }
    var path: MutableList<String> = mutableListOf<String>()
    var n: Int = (0).toInt()
    while (n < observations.size) {
        path = run { val _tmp = path.toMutableList(); _tmp.add(""); _tmp }
        n = n + 1
    }
    var last_obs: String = observations[observations.size - 1]!!
    var max_final: Double = 0.0 - 1.0
    var last_state: String = ""
    var m: Int = (0).toInt()
    while (m < states.size) {
        var state: String = states[m]!!
        var prob: Double = (probs)[key(state, last_obs)] as Double
        if (prob > max_final) {
            max_final = prob
            last_state = state
        }
        m = m + 1
    }
    var last_index: Int = (observations.size - 1).toInt()
    _listSet(path, last_index, last_state)
    var idx: Int = (last_index).toInt()
    while (idx > 0) {
        var obs: String = observations[idx]!!
        var prev: String = (ptrs)[key(path[idx]!!, obs)] as String
        _listSet(path, idx - 1, prev)
        idx = idx - 1
    }
    return path
}

fun join_words(words: MutableList<String>): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < words.size) {
        if (i > 0) {
            res = res + " "
        }
        res = res + words[i]!!
        i = i + 1
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(join_words(result))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
