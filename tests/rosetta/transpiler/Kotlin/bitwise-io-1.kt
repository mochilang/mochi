import java.math.BigInteger

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

data class Writer(var order: String = "", var bits: Int = 0, var nbits: Int = 0, var data: MutableList<Int> = mutableListOf<Int>())
fun pow2(n: Int): Int {
    var v: Int = 1
    var i: Int = 0
    while (i < n) {
        v = v * 2
        i = i + 1
    }
    return v
}

fun lshift(x: Int, n: Int): Int {
    return x * pow2(n)
}

fun rshift(x: Int, n: Int): Int {
    return x / pow2(n)
}

fun NewWriter(order: String): Writer {
    return Writer(order = order, bits = 0, nbits = 0, data = mutableListOf<Int>())
}

fun writeBitsLSB(w: Writer, c: Int, width: Int): Writer {
    w.bits = w.bits + lshift(c, w.nbits)
    w.nbits = w.nbits + width
    while (w.nbits >= 8) {
        var b: BigInteger = (Math.floorMod(w.bits, 256)).toBigInteger()
        w.data = run { val _tmp = (w.data).toMutableList(); _tmp.add((b.toInt())); _tmp }
        w.bits = rshift(w.bits, 8)
        w.nbits = w.nbits - 8
    }
    return w
}

fun writeBitsMSB(w: Writer, c: Int, width: Int): Writer {
    w.bits = w.bits + lshift(c, (32 - width) - w.nbits)
    w.nbits = w.nbits + width
    while (w.nbits >= 8) {
        var b: BigInteger = (Math.floorMod(rshift(w.bits, 24), 256)).toBigInteger()
        w.data = run { val _tmp = (w.data).toMutableList(); _tmp.add((b.toInt())); _tmp }
        w.bits = (Math.floorMod(w.bits, pow2(24))) * 256
        w.nbits = w.nbits - 8
    }
    return w
}

fun WriteBits(w: Writer, c: Int, width: Int): Writer {
    if (w.order == "LSB") {
        return writeBitsLSB(w, c, width)
    }
    return writeBitsMSB(w, c, width)
}

fun CloseWriter(w: Writer): Writer {
    if (w.nbits > 0) {
        if (w.order == "MSB") {
            w.bits = rshift(w.bits, 24)
        }
        w.data = run { val _tmp = (w.data).toMutableList(); _tmp.add(Math.floorMod(w.bits, 256)); _tmp }
    }
    w.bits = 0
    w.nbits = 0
    return w
}

fun toBinary(n: Int, bits: Int): String {
    var b: String = ""
    var _val: Int = n
    var i: Int = 0
    while (i < bits) {
        b = (Math.floorMod(_val, 2)).toString() + b
        _val = _val / 2
        i = i + 1
    }
    return b
}

fun bytesToBits(bs: MutableList<Int>): String {
    var out: String = "["
    var i: Int = 0
    while (i < bs.size) {
        out = out + toBinary(bs[i]!!, 8)
        if ((i + 1) < bs.size) {
            out = out + " "
        }
        i = i + 1
    }
    out = out + "]"
    return out
}

fun ExampleWriter_WriteBits(): Unit {
    var bw: Writer = NewWriter("MSB")
    bw = WriteBits(bw, 15, 4)
    bw = WriteBits(bw, 0, 1)
    bw = WriteBits(bw, 19, 5)
    bw = CloseWriter(bw)
    println(bytesToBits(bw.data))
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        ExampleWriter_WriteBits()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
