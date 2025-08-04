import java.math.BigInteger

fun pow2(n: Int): Long {
var v = 1L
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

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

var t: BigInteger = (_now() / 1000000000).toBigInteger()
var sec: BigInteger = t.remainder((60).toBigInteger())
var mins: BigInteger = t.divide((60).toBigInteger())
var min: BigInteger = mins.remainder((60).toBigInteger())
var hour: BigInteger = (mins.divide((60).toBigInteger())).remainder((24).toBigInteger())
fun bin(n: Int, digits: Int): String {
    var n: Int = n
    var s: String = ""
    var i: BigInteger = ((digits - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        var p: Int = ((pow2((i.toInt()))).toInt())
        if (n >= p) {
            s = s + "x"
            n = n - p
        } else {
            s = s + " "
        }
        if (i.compareTo((0).toBigInteger()) > 0) {
            s = s + "|"
        }
        i = i.subtract((1).toBigInteger())
    }
    return s
}

fun main() {
    println(bin((hour.toInt()), 8))
    println("")
    println(bin((min.toInt()), 8))
    println("")
    var xs: String = ""
    var i: Int = 0
    while ((i).toBigInteger().compareTo((sec)) < 0) {
        xs = xs + "x"
        i = i + 1
    }
    var out: String = ""
    var j: Int = 0
    while (j < xs.length) {
        out = out + xs.substring(j, j + 1)
        if (((Math.floorMod((j + 1), 5)) == 0) && ((j + 1) < xs.length)) {
            out = out + "|"
        }
        j = j + 1
    }
    println(out)
}
