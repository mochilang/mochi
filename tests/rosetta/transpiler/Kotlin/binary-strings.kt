import java.math.BigInteger

var b: MutableList<Int> = mutableListOf(98, 105, 110, 97, 114, 121)
fun char(n: Int): String {
    var letters: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: BigInteger = (n - 97).toBigInteger()
    if ((idx.compareTo((0).toBigInteger()) < 0) || (idx.compareTo((letters.length).toBigInteger()) >= 0)) {
        return "?"
    }
    return letters.substring((idx).toInt(), (idx.add((1).toBigInteger())).toInt())
}

fun fromBytes(bs: MutableList<Int>): String {
    var s: String = ""
    var i: Int = 0
    while (i < bs.size) {
        s = s + char(bs[i]!!)
        i = i + 1
    }
    return s
}

fun main() {
    println(b.toString())
    var c: MutableList<Int> = b
    println(c.toString())
    println((b == c).toString())
    var d: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < b.size) {
        d = run { val _tmp = d.toMutableList(); _tmp.add(b[i]!!); _tmp }
        i = i + 1
    }
    d[1] = 97
    d[4] = 110
    println(fromBytes(b))
    println(fromBytes(d))
    println((b.size == 0).toString())
    var z: MutableList<Any?> = run { val _tmp = b.toMutableList(); _tmp.add(122); _tmp }
    println(fromBytes((z as MutableList<Int>)))
    var sub: MutableList<Int> = b.subList(1, 3)
    println(fromBytes(sub))
    var f: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < d.size) {
        var _val: Int = d[i]!!
        if (_val == 110) {
            f = run { val _tmp = f.toMutableList(); _tmp.add(109); _tmp }
        } else {
            f = run { val _tmp = f.toMutableList(); _tmp.add(_val); _tmp }
        }
        i = i + 1
    }
    println((fromBytes(d) + " -> ") + fromBytes(f))
    var rem: MutableList<Int> = mutableListOf<Int>()
    rem = run { val _tmp = rem.toMutableList(); _tmp.add(b[0]!!); _tmp }
    i = 3
    while (i < b.size) {
        rem = run { val _tmp = rem.toMutableList(); _tmp.add(b[i]!!); _tmp }
        i = i + 1
    }
    println(fromBytes(rem))
}
