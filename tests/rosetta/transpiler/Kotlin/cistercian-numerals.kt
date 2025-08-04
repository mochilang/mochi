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

var n: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
var draw: MutableMap<Int, () -> Any> = mutableMapOf<Int, () -> Any>()
fun initN(): Unit {
    var i: Int = 0
    while (i < 15) {
        var row: MutableList<String> = mutableListOf<String>()
        var j: Int = 0
        while (j < 11) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(" "); _tmp }
            j = j + 1
        }
        row[5] = "x"
        n = run { val _tmp = n.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
}

fun horiz(c1: Int, c2: Int, r: Int): Unit {
    var c: Int = c1
    while (c <= c2) {
        (n[r]!!)[c] = "x"
        c = c + 1
    }
}

fun verti(r1: Int, r2: Int, c: Int): Unit {
    var r: Int = r1
    while (r <= r2) {
        (n[r]!!)[c] = "x"
        r = r + 1
    }
}

fun diagd(c1: Int, c2: Int, r: Int): Unit {
    var c: Int = c1
    while (c <= c2) {
        (n[(r + c) - c1]!!)[c] = "x"
        c = c + 1
    }
}

fun diagu(c1: Int, c2: Int, r: Int): Unit {
    var c: Int = c1
    while (c <= c2) {
        (n[(r - c) + c1]!!)[c] = "x"
        c = c + 1
    }
}

fun initDraw(): Unit {
    (draw)[1] = ({horiz(6, 10, 0) } as () -> Any)
    (draw)[2] = ({horiz(6, 10, 4) } as () -> Any)
    (draw)[3] = ({diagd(6, 10, 0) } as () -> Any)
    (draw)[4] = ({diagu(6, 10, 4) } as () -> Any)
    (draw)[5] = ({
    ((draw)[1] as () -> Any)()
    ((draw)[4] as () -> Any)()
} as () -> Any)
    (draw)[6] = ({verti(0, 4, 10) } as () -> Any)
    (draw)[7] = ({
    ((draw)[1] as () -> Any)()
    ((draw)[6] as () -> Any)()
} as () -> Any)
    (draw)[8] = ({
    ((draw)[2] as () -> Any)()
    ((draw)[6] as () -> Any)()
} as () -> Any)
    (draw)[9] = ({
    ((draw)[1] as () -> Any)()
    ((draw)[8] as () -> Any)()
} as () -> Any)
    (draw)[10] = ({horiz(0, 4, 0) } as () -> Any)
    (draw)[20] = ({horiz(0, 4, 4) } as () -> Any)
    (draw)[30] = ({diagu(0, 4, 4) } as () -> Any)
    (draw)[40] = ({diagd(0, 4, 0) } as () -> Any)
    (draw)[50] = ({
    ((draw)[10] as () -> Any)()
    ((draw)[40] as () -> Any)()
} as () -> Any)
    (draw)[60] = ({verti(0, 4, 0) } as () -> Any)
    (draw)[70] = ({
    ((draw)[10] as () -> Any)()
    ((draw)[60] as () -> Any)()
} as () -> Any)
    (draw)[80] = ({
    ((draw)[20] as () -> Any)()
    ((draw)[60] as () -> Any)()
} as () -> Any)
    (draw)[90] = ({
    ((draw)[10] as () -> Any)()
    ((draw)[80] as () -> Any)()
} as () -> Any)
    (draw)[100] = ({horiz(6, 10, 14) } as () -> Any)
    (draw)[200] = ({horiz(6, 10, 10) } as () -> Any)
    (draw)[300] = ({diagu(6, 10, 14) } as () -> Any)
    (draw)[400] = ({diagd(6, 10, 10) } as () -> Any)
    (draw)[500] = ({
    ((draw)[100] as () -> Any)()
    ((draw)[400] as () -> Any)()
} as () -> Any)
    (draw)[600] = ({verti(10, 14, 10) } as () -> Any)
    (draw)[700] = ({
    ((draw)[100] as () -> Any)()
    ((draw)[600] as () -> Any)()
} as () -> Any)
    (draw)[800] = ({
    ((draw)[200] as () -> Any)()
    ((draw)[600] as () -> Any)()
} as () -> Any)
    (draw)[900] = ({
    ((draw)[100] as () -> Any)()
    ((draw)[800] as () -> Any)()
} as () -> Any)
    (draw)[1000] = ({horiz(0, 4, 14) } as () -> Any)
    (draw)[2000] = ({horiz(0, 4, 10) } as () -> Any)
    (draw)[3000] = ({diagd(0, 4, 10) } as () -> Any)
    (draw)[4000] = ({diagu(0, 4, 14) } as () -> Any)
    (draw)[5000] = ({
    ((draw)[1000] as () -> Any)()
    ((draw)[4000] as () -> Any)()
} as () -> Any)
    (draw)[6000] = ({verti(10, 14, 0) } as () -> Any)
    (draw)[7000] = ({
    ((draw)[1000] as () -> Any)()
    ((draw)[6000] as () -> Any)()
} as () -> Any)
    (draw)[8000] = ({
    ((draw)[2000] as () -> Any)()
    ((draw)[6000] as () -> Any)()
} as () -> Any)
    (draw)[9000] = ({
    ((draw)[1000] as () -> Any)()
    ((draw)[8000] as () -> Any)()
} as () -> Any)
}

fun printNumeral(): Unit {
    var i: Int = 0
    while (i < 15) {
        var line: String = ""
        var j: Int = 0
        while (j < 11) {
            line = (line + (((n[i]!!) as MutableList<String>))[j]!!) + " "
            j = j + 1
        }
        println(line)
        i = i + 1
    }
    println("")
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        initDraw()
        var numbers: MutableList<Int> = mutableListOf(0, 1, 20, 300, 4000, 5555, 6789, 9999)
        for (number in numbers) {
            initN()
            println(number.toString() + ":")
            var num: Int = number
            var thousands: Int = num / 1000
            num = Math.floorMod(num, 1000)
            var hundreds: Int = num / 100
            num = Math.floorMod(num, 100)
            var tens: Int = num / 10
            var ones: Int = Math.floorMod(num, 10)
            if (thousands > 0) {
                ((draw)[thousands * 1000] as () -> Any)()
            }
            if (hundreds > 0) {
                ((draw)[hundreds * 100] as () -> Any)()
            }
            if (tens > 0) {
                ((draw)[tens * 10] as () -> Any)()
            }
            if (ones > 0) {
                ((draw)[ones] as () -> Any)()
            }
            printNumeral()
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
