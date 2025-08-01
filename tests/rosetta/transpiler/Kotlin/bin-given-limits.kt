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

fun getBins(limits: MutableList<Int>, data: MutableList<Int>): MutableList<Int> {
    var n: Int = limits.size
    var bins: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < (n + 1)) {
        bins = run { val _tmp = bins.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
        i = i + 1
    }
    var j: Int = 0
    while (j < data.size) {
        var d: Int = data[j]!!
        var index: Int = 0
        while (index < limits.size) {
            if (d < limits[index]!!) {
                break
            }
            if (d == limits[index]!!) {
                index = index + 1
                break
            }
            index = index + 1
        }
        bins[index] = bins[index]!! + 1
        j = j + 1
    }
    return bins
}

fun padLeft(n: Int, width: Int): String {
    var s: String = n.toString()
    var pad: BigInteger = (width - s.length).toBigInteger()
    var out: String = ""
    var i: Int = 0
    while ((i).toBigInteger().compareTo((pad)) < 0) {
        out = out + " "
        i = i + 1
    }
    return out + s
}

fun printBins(limits: MutableList<Int>, bins: MutableList<Int>): Unit {
    var n: Int = limits.size
    println((("           < " + padLeft(limits[0]!!, 3)) + " = ") + padLeft(bins[0]!!, 2))
    var i: Int = 1
    while (i < n) {
        println(((((">= " + padLeft(limits[i - 1]!!, 3)) + " and < ") + padLeft(limits[i]!!, 3)) + " = ") + padLeft(bins[i]!!, 2))
        i = i + 1
    }
    println(((">= " + padLeft(limits[n - 1]!!, 3)) + "           = ") + padLeft(bins[n]!!, 2))
    println("")
}

fun user_main(): Unit {
    var limitsList: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(23, 37, 43, 53, 67, 83), mutableListOf(14, 18, 249, 312, 389, 392, 513, 591, 634, 720))
    var dataList: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(95, 21, 94, 12, 99, 4, 70, 75, 83, 93, 52, 80, 57, 5, 53, 86, 65, 17, 92, 83, 71, 61, 54, 58, 47, 16, 8, 9, 32, 84, 7, 87, 46, 19, 30, 37, 96, 6, 98, 40, 79, 97, 45, 64, 60, 29, 49, 36, 43, 55), mutableListOf(445, 814, 519, 697, 700, 130, 255, 889, 481, 122, 932, 77, 323, 525, 570, 219, 367, 523, 442, 933, 416, 589, 930, 373, 202, 253, 775, 47, 731, 685, 293, 126, 133, 450, 545, 100, 741, 583, 763, 306, 655, 267, 248, 477, 549, 238, 62, 678, 98, 534, 622, 907, 406, 714, 184, 391, 913, 42, 560, 247, 346, 860, 56, 138, 546, 38, 985, 948, 58, 213, 799, 319, 390, 634, 458, 945, 733, 507, 916, 123, 345, 110, 720, 917, 313, 845, 426, 9, 457, 628, 410, 723, 354, 895, 881, 953, 677, 137, 397, 97, 854, 740, 83, 216, 421, 94, 517, 479, 292, 963, 376, 981, 480, 39, 257, 272, 157, 5, 316, 395, 787, 942, 456, 242, 759, 898, 576, 67, 298, 425, 894, 435, 831, 241, 989, 614, 987, 770, 384, 692, 698, 765, 331, 487, 251, 600, 879, 342, 982, 527, 736, 795, 585, 40, 54, 901, 408, 359, 577, 237, 605, 847, 353, 968, 832, 205, 838, 427, 876, 959, 686, 646, 835, 127, 621, 892, 443, 198, 988, 791, 466, 23, 707, 467, 33, 670, 921, 180, 991, 396, 160, 436, 717, 918, 8, 374, 101, 684, 727, 749))
    var i: Int = 0
    while (i < limitsList.size) {
        println(("Example " + (i + 1).toString()) + "\n")
        var bins: MutableList<Int> = getBins(limitsList[i]!!, dataList[i]!!)
        printBins(limitsList[i]!!, bins)
        i = i + 1
    }
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
