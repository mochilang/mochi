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

fun square_to_maps(square: MutableList<MutableList<String>>): MutableMap<String, Any?> {
    var emap: MutableMap<String, MutableList<Int>> = mutableMapOf<Any?, Any?>() as MutableMap<String, MutableList<Int>>
    var dmap: MutableMap<String, String> = mutableMapOf<Any?, Any?>() as MutableMap<String, String>
    var x: Int = 0
    while (x < square.size) {
        val row: MutableList<String> = square[x]
        var y: Int = 0
        while (y < row.size) {
            val ch: String = row[y]
            (emap)[ch] = mutableListOf(x, y)
            (dmap)[(x.toString() + ",") + y.toString()] = ch
            y = y + 1
        }
        x = x + 1
    }
    return mutableMapOf<String, Any?>("e" to (emap), "d" to (dmap))
}

fun remove_space(text: String, emap: MutableMap<String, MutableList<Int>>): String {
    val s: String = text.toUpperCase()
    var out: String = ""
    var i: Int = 0
    while (i < s.length) {
        val ch: String = s.substring(i, i + 1)
        if ((ch != " ") && (ch in emap)) {
            out = out + ch
        }
        i = i + 1
    }
    return out
}

fun encrypt(text: String, emap: MutableMap<String, MutableList<Int>>, dmap: MutableMap<String, String>): String {
    var text: String = text
    text = remove_space(text, emap)
    var row0: MutableList<Int> = mutableListOf<Int>()
    var row1: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < text.length) {
        val ch: String = text.substring(i, i + 1)
        val xy: MutableList<Int> = (emap)[ch] as MutableList<Int>
        row0 = run { val _tmp = row0.toMutableList(); _tmp.add(xy[0]); _tmp } as MutableList<Int>
        row1 = run { val _tmp = row1.toMutableList(); _tmp.add(xy[1]); _tmp } as MutableList<Int>
        i = i + 1
    }
    for (v in row1) {
        row0 = run { val _tmp = row0.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
    }
    var res: String = ""
    var j: Int = 0
    while (j < row0.size) {
        val key: String = ((row0[j]).toString() + ",") + (row0[j + 1]).toString()
        res = res + (dmap)[key] as String
        j = j + 2
    }
    return res
}

fun decrypt(text: String, emap: MutableMap<String, MutableList<Int>>, dmap: MutableMap<String, String>): String {
    var text: String = text
    text = remove_space(text, emap)
    var coords: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < text.length) {
        val ch: String = text.substring(i, i + 1)
        val xy: MutableList<Int> = (emap)[ch] as MutableList<Int>
        coords = run { val _tmp = coords.toMutableList(); _tmp.add(xy[0]); _tmp } as MutableList<Int>
        coords = run { val _tmp = coords.toMutableList(); _tmp.add(xy[1]); _tmp } as MutableList<Int>
        i = i + 1
    }
    var half: Int = coords.size / 2
    var k1: MutableList<Int> = mutableListOf<Int>()
    var k2: MutableList<Int> = mutableListOf<Int>()
    var idx: Int = 0
    while (idx < half) {
        k1 = run { val _tmp = k1.toMutableList(); _tmp.add(coords[idx]); _tmp } as MutableList<Int>
        idx = idx + 1
    }
    while (idx < coords.size) {
        k2 = run { val _tmp = k2.toMutableList(); _tmp.add(coords[idx]); _tmp } as MutableList<Int>
        idx = idx + 1
    }
    var res: String = ""
    var j: Int = 0
    while (j < half) {
        val key: String = ((k1[j]).toString() + ",") + (k2[j]).toString()
        res = res + (dmap)[key] as String
        j = j + 1
    }
    return res
}

fun user_main(): Unit {
    val squareRosetta: MutableList<MutableList<String>> = mutableListOf(mutableListOf("A", "B", "C", "D", "E"), mutableListOf("F", "G", "H", "I", "K"), mutableListOf("L", "M", "N", "O", "P"), mutableListOf("Q", "R", "S", "T", "U"), mutableListOf("V", "W", "X", "Y", "Z"), mutableListOf("J", "1", "2", "3", "4"))
    val squareWikipedia: MutableList<MutableList<String>> = mutableListOf(mutableListOf("B", "G", "W", "K", "Z"), mutableListOf("Q", "P", "N", "D", "S"), mutableListOf("I", "O", "A", "X", "E"), mutableListOf("F", "C", "L", "U", "M"), mutableListOf("T", "H", "Y", "V", "R"), mutableListOf("J", "1", "2", "3", "4"))
    val textRosetta: String = "0ATTACKATDAWN"
    val textWikipedia: String = "FLEEATONCE"
    val textTest: String = "The invasion will start on the first of January"
    var maps: MutableMap<String, Any?> = square_to_maps(squareRosetta)
    var emap: Any? = (maps)["e"] as Any?
    var dmap: Any? = (maps)["d"] as Any?
    println("from Rosettacode")
    println("original:\t " + textRosetta)
    var s: String = encrypt(textRosetta, emap as MutableMap<String, MutableList<Int>>, dmap as MutableMap<String, String>)
    println("codiert:\t " + s)
    s = decrypt(s, emap as MutableMap<String, MutableList<Int>>, dmap as MutableMap<String, String>)
    println("and back:\t " + s)
    maps = square_to_maps(squareWikipedia)
    emap = (maps)["e"] as Any?
    dmap = (maps)["d"] as Any?
    println("from Wikipedia")
    println("original:\t " + textWikipedia)
    s = encrypt(textWikipedia, emap as MutableMap<String, MutableList<Int>>, dmap as MutableMap<String, String>)
    println("codiert:\t " + s)
    s = decrypt(s, emap as MutableMap<String, MutableList<Int>>, dmap as MutableMap<String, String>)
    println("and back:\t " + s)
    maps = square_to_maps(squareWikipedia)
    emap = (maps)["e"] as Any?
    dmap = (maps)["d"] as Any?
    println("from Rosettacode long part")
    println("original:\t " + textTest)
    s = encrypt(textTest, emap as MutableMap<String, MutableList<Int>>, dmap as MutableMap<String, String>)
    println("codiert:\t " + s)
    s = decrypt(s, emap as MutableMap<String, MutableList<Int>>, dmap as MutableMap<String, String>)
    println("and back:\t " + s)
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
