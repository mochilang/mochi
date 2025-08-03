import java.math.BigInteger

var width: Int = 81
var height: Int = 5
var lines: MutableList<String> = mutableListOf<String>()
var stack: MutableList<MutableMap<String, Int>> = mutableListOf(mutableMapOf<String, Int>("start" to (0), "len" to (width), "index" to (1)))
fun setChar(s: String, idx: Int, ch: String): String {
    return (s.substring(0, idx) + ch) + s.substring(idx + 1, s.length)
}

fun main() {
    for (i in 0 until height) {
        var row: String = ""
        var j: Int = 0
        while (j < width) {
            row = row + "*"
            j = j + 1
        }
        lines = run { val _tmp = lines.toMutableList(); _tmp.add(row); _tmp }
    }
    while (stack.size > 0) {
        var frame: MutableMap<String, Int> = stack[stack.size - 1]!!
        stack = stack.subList(0, stack.size - 1)
        var start: Int = (frame)["start"] as Int
        var lenSeg: Int = (frame)["len"] as Int
        var index: Int = (frame)["index"] as Int
        var seg: Int = ((lenSeg / 3).toInt())
        if (seg == 0) {
            continue
        }
        var i: Int = index
        while (i < height) {
            var j: BigInteger = (start + seg).toBigInteger()
            while (j.compareTo((start + (2 * seg)).toBigInteger()) < 0) {
                lines[i] = setChar(lines[i]!!, (j.toInt()), " ")
                j = j.add((1).toBigInteger())
            }
            i = i + 1
        }
        stack = run { val _tmp = stack.toMutableList(); _tmp.add(mutableMapOf<String, Int>("start" to (start), "len" to (seg), "index" to (index + 1))); _tmp }
        stack = run { val _tmp = stack.toMutableList(); _tmp.add(mutableMapOf<String, Int>("start" to (start + (seg * 2)), "len" to (seg), "index" to (index + 1))); _tmp }
    }
    for (line in lines) {
        println(line)
    }
}
