data class cds(var i: Int = 0, var s: String = "", var b: MutableList<Int> = mutableListOf<Int>(), var m: MutableMap<Int, Boolean> = mutableMapOf<Int, Boolean>())
var c1: cds = cds(i = 1, s = "one", b = mutableListOf(117, 110, 105, 116), m = (mutableMapOf<Int, Boolean>(1 to (true)) as MutableMap<Int, Boolean>))
var c2: cds = deepcopy(c1)
fun copyList(src: MutableList<Int>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf<Int>()
    for (v in src) {
        out = run { val _tmp = out.toMutableList(); _tmp.add(v); _tmp }
    }
    return out
}

fun copyMap(src: MutableMap<Int, Boolean>): MutableMap<Int, Boolean> {
    var out: MutableMap<Int, Boolean> = mutableMapOf<Int, Boolean>()
    for (k in src.keys) {
        (out)[k] = (src)[k] ?: false
    }
    return out
}

fun deepcopy(c: cds): cds {
    return cds(i = c.i, s = c.s, b = copyList(c.b), m = copyMap(c.m))
}

fun cdsStr(c: cds): String {
    var bs: String = "["
    var i: Int = 0
    while (i < (c.b).size) {
        bs = bs + ((c.b)[i]!!).toString()
        if (i < ((c.b).size - 1)) {
            bs = bs + " "
        }
        i = i + 1
    }
    bs = bs + "]"
    var ms: String = "map["
    var first: Boolean = true
    for (k in c.m.keys) {
        if (!first) {
            ms = ms + " "
        }
        ms = ((ms + k.toString()) + ":") + ((c.m)[k] ?: false).toString()
        first = false
    }
    ms = ms + "]"
    return ((((((("{" + c.i.toString()) + " ") + c.s) + " ") + bs) + " ") + ms) + "}"
}

fun main() {
    println(cdsStr(c1))
    println(cdsStr(c2))
    c1 = cds(i = 0, s = "nil", b = mutableListOf(122, 101, 114, 111), m = (mutableMapOf<Int, Boolean>(1 to (false)) as MutableMap<Int, Boolean>))
    println(cdsStr(c1))
    println(cdsStr(c2))
}
