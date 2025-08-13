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

var query: String = "HEAGAWGHEE"
var subject: String = "PAWHEAE"
var score: MutableList<MutableList<Int>> = smith_waterman(query, subject, 1, 0 - 1, 0 - 2)
fun score_function(source_char: String, target_char: String, match_score: Int, mismatch_score: Int, gap_score: Int): Int {
    if ((source_char == "-") || (target_char == "-")) {
        return gap_score
    }
    if (source_char == target_char) {
        return match_score
    }
    return mismatch_score
}

fun smith_waterman(query: String, subject: String, match_score: Int, mismatch_score: Int, gap_score: Int): MutableList<MutableList<Int>> {
    var q: String = (query.toUpperCase() as String)
    var s: String = (subject.toUpperCase() as String)
    var m: Int = (q.length).toInt()
    var n: Int = (s.length).toInt()
    var score: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (_u1 in 0 until m + 1) {
        var row: MutableList<Int> = mutableListOf<Int>()
        for (_2 in 0 until n + 1) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
        }
        score = run { val _tmp = score.toMutableList(); _tmp.add(row); _tmp }
    }
    for (i in 1 until m + 1) {
        for (j in 1 until n + 1) {
            var qc: String = q.substring(i - 1, i)
            var sc: String = s.substring(j - 1, j)
            var diag: Int = ((((score[i - 1]!!) as MutableList<Int>))[j - 1]!! + score_function(qc, sc, match_score, mismatch_score, gap_score)).toInt()
            var delete: Int = ((((score[i - 1]!!) as MutableList<Int>))[j]!! + gap_score).toInt()
            var insert: Int = ((((score[i]!!) as MutableList<Int>))[j - 1]!! + gap_score).toInt()
            var max_val: Int = (0).toInt()
            if (diag > max_val) {
                max_val = diag
            }
            if (delete > max_val) {
                max_val = delete
            }
            if (insert > max_val) {
                max_val = insert
            }
            _listSet(score[i]!!, j, max_val)
        }
    }
    return score
}

fun traceback(score: MutableList<MutableList<Int>>, query: String, subject: String, match_score: Int, mismatch_score: Int, gap_score: Int): String {
    var q: String = (query.toUpperCase() as String)
    var s: String = (subject.toUpperCase() as String)
    var max_value: Int = (0).toInt()
    var i_max: Int = (0).toInt()
    var j_max: Int = (0).toInt()
    for (i in 0 until score.size) {
        for (j in 0 until (score[i]!!).size) {
            if ((((score[i]!!) as MutableList<Int>))[j]!! > max_value) {
                max_value = (((score[i]!!) as MutableList<Int>))[j]!!
                i_max = i
                j_max = j
            }
        }
    }
    var i: Int = (i_max).toInt()
    var j: Int = (j_max).toInt()
    var align1: String = ""
    var align2: String = ""
    var gap_penalty: Int = (score_function("-", "-", match_score, mismatch_score, gap_score)).toInt()
    if ((i == 0) || (j == 0)) {
        return ""
    }
    while ((i > 0) && (j > 0)) {
        var qc: String = q.substring(i - 1, i)
        var sc: String = s.substring(j - 1, j)
        if ((((score[i]!!) as MutableList<Int>))[j]!! == ((((score[i - 1]!!) as MutableList<Int>))[j - 1]!! + score_function(qc, sc, match_score, mismatch_score, gap_score))) {
            align1 = qc + align1
            align2 = sc + align2
            i = i - 1
            j = j - 1
        } else {
            if ((((score[i]!!) as MutableList<Int>))[j]!! == ((((score[i - 1]!!) as MutableList<Int>))[j]!! + gap_penalty)) {
                align1 = qc + align1
                align2 = "-" + align2
                i = i - 1
            } else {
                align1 = "-" + align1
                align2 = sc + align2
                j = j - 1
            }
        }
    }
    return (align1 + "\n") + align2
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(traceback(score, query, subject, 1, 0 - 1, 0 - 2))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
