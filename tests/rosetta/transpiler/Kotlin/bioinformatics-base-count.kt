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

var dna: String = ((((((((("" + "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATG") + "CTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTG") + "AGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGAT") + "GGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT") + "CGATTCTGCTTATAACACTATGTTCTTATGAAATGGATGTTCTGAGTTGG") + "TCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA") + "TTTAATTTTTCTATATAGCGATCTGTATTTAAGCAATTCATTTAGGTTAT") + "CGCCGCGATGCTCGGTTCGGACCGCCAAGCATCTGGCTCCACTGCTAGTG") + "TCCTAAATTTGAATGGCAAACACAAATAAGATTTAGCAATTCGTGTAGAC") + "GACCGGGGACTTGCATGATGGGAGCAGCTTTGTTAAACTACGAACGTAAT"
fun padLeft(s: String, w: Int): String {
    var res: String = ""
    var n: BigInteger = (w - s.length).toBigInteger()
    while (n.compareTo((0).toBigInteger()) > 0) {
        res = res + " "
        n = n.subtract((1).toBigInteger())
    }
    return res + s
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("SEQUENCE:")
        var le: Int = dna.length
        var i: Int = 0
        while (i < le) {
            var k: BigInteger = (i + 50).toBigInteger()
            if (k.compareTo((le).toBigInteger()) > 0) {
                k = le.toBigInteger()
            }
            println((padLeft(i.toString(), 5) + ": ") + dna.substring(i, (k).toInt()))
            i = i + 50
        }
        var a: Int = 0
        var c: Int = 0
        var g: Int = 0
        var t: Int = 0
        var idx: Int = 0
        while (idx < le) {
            var ch: String = dna.substring(idx, idx + 1)
            if (ch == "A") {
                a = a + 1
            } else {
                if (ch == "C") {
                    c = c + 1
                } else {
                    if (ch == "G") {
                        g = g + 1
                    } else {
                        if (ch == "T") {
                            t = t + 1
                        }
                    }
                }
            }
            idx = idx + 1
        }
        println("")
        println("BASE COUNT:")
        println("    A: " + padLeft(a.toString(), 3))
        println("    C: " + padLeft(c.toString(), 3))
        println("    G: " + padLeft(g.toString(), 3))
        println("    T: " + padLeft(t.toString(), 3))
        println("    ------")
        println("    Σ: " + le.toString())
        println("    ======")
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
