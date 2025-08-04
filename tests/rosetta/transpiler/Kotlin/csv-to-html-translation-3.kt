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

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("<table>")
        println("   <thead>")
        println("      <tr><th>Character</th><th>Speech</th></tr>")
        println("   </thead>")
        println("   <tbody>")
        println("      <tr><td>The multitude</td><td>The messiah! Show us the messiah!</td></tr>")
        println("      <tr><td>Brians mother</td><td>&lt;angry&gt;Now you listen here! He&#39;s not the messiah; he&#39;s a very naughty boy! Now go away!&lt;/angry&gt;</td></tr>")
        println("      <tr><td>The multitude</td><td>Who are you?</td></tr>")
        println("      <tr><td>Brians mother</td><td>I&#39;m his mother; that&#39;s who!</td></tr>")
        println("      <tr><td>The multitude</td><td>Behold his mother! Behold his mother!</td></tr>")
        println("   </tbody>")
        println("</table>")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
