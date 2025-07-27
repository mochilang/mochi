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

fun user_main(): Unit {
    println("Diagram after trimming whitespace and removal of blank lines:\n")
    println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
    println("|                      ID                       |")
    println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
    println("|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |")
    println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
    println("|                    QDCOUNT                    |")
    println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
    println("|                    ANCOUNT                    |")
    println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
    println("|                    NSCOUNT                    |")
    println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
    println("|                    ARCOUNT                    |")
    println("+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+")
    println("\nDecoded:\n")
    println("Name     Bits  Start  End")
    println("=======  ====  =====  ===")
    println("ID        16      0    15")
    println("QR         1     16    16")
    println("Opcode     4     17    20")
    println("AA         1     21    21")
    println("TC         1     22    22")
    println("RD         1     23    23")
    println("RA         1     24    24")
    println("Z          3     25    27")
    println("RCODE      4     28    31")
    println("QDCOUNT   16     32    47")
    println("ANCOUNT   16     48    63")
    println("NSCOUNT   16     64    79")
    println("ARCOUNT   16     80    95")
    println("\nTest string in hex:")
    println("78477bbf5496e12e1bf169a4")
    println("\nTest string in binary:")
    println("011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100")
    println("\nUnpacked:\n")
    println("Name     Size  Bit pattern")
    println("=======  ====  ================")
    println("ID        16   0111100001000111")
    println("QR         1   0")
    println("Opcode     4   1111")
    println("AA         1   0")
    println("TC         1   1")
    println("RD         1   1")
    println("RA         1   1")
    println("Z          3   011")
    println("RCODE      4   1111")
    println("QDCOUNT   16   0101010010010110")
    println("ANCOUNT   16   1110000100101110")
    println("NSCOUNT   16   0001101111110001")
    println("ARCOUNT   16   0110100110100100")
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
