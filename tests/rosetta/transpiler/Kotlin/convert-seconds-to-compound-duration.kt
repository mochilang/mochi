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

fun timeStr(sec: Int): String {
    var sec: Int = sec
    var wks: Int = sec / 604800
    sec = Math.floorMod(sec, 604800)
    var ds: Int = sec / 86400
    sec = Math.floorMod(sec, 86400)
    var hrs: Int = sec / 3600
    sec = Math.floorMod(sec, 3600)
    var mins: Int = sec / 60
    sec = Math.floorMod(sec, 60)
    var res: String = ""
    var comma: Boolean = false
    if (wks != 0) {
        res = (res + wks.toString()) + " wk"
        comma = true
    }
    if (ds != 0) {
        if ((comma as Boolean)) {
            res = res + ", "
        }
        res = (res + ds.toString()) + " d"
        comma = true
    }
    if (hrs != 0) {
        if ((comma as Boolean)) {
            res = res + ", "
        }
        res = (res + hrs.toString()) + " hr"
        comma = true
    }
    if (mins != 0) {
        if ((comma as Boolean)) {
            res = res + ", "
        }
        res = (res + mins.toString()) + " min"
        comma = true
    }
    if (sec != 0) {
        if ((comma as Boolean)) {
            res = res + ", "
        }
        res = (res + sec.toString()) + " sec"
    }
    return res
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(timeStr(7259))
        println(timeStr(86400))
        println(timeStr(6000000))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
