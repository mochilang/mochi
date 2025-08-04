import java.math.BigInteger

data class MDRResult(var mp: Int = 0, var mdr: Int = 0)
fun pad(s: String, width: Int): String {
    var out: String = s
    while (out.length < width) {
        out = " " + out
    }
    return out
}

fun mult(n: BigInteger, base: Int): BigInteger {
    var m: BigInteger = java.math.BigInteger.valueOf(1)
    var x: BigInteger = n
    var b: BigInteger = (base.toBigInteger())
    while (x.compareTo(((0.toBigInteger()))) > 0) {
        m = m.multiply((x.remainder((b))))
        x = x.divide((b))
    }
    return m
}

fun multDigitalRoot(n: BigInteger, base: Int): MDRResult {
    var m: BigInteger = n
    var mp: Int = 0
    var b: BigInteger = (base.toBigInteger())
    while (m.compareTo((b)) >= 0) {
        m = mult(m, base)
        mp = mp + 1
    }
    return MDRResult(mp = mp, mdr = (m.toInt()))
}

fun user_main(): Unit {
    var base: Int = 10
    var size: Int = 5
    println((((pad("Number", 20) + " ") + pad("MDR", 3)) + " ") + pad("MP", 3))
    var nums: MutableList<BigInteger> = mutableListOf((123321.toBigInteger()), (7739.toBigInteger()), (893.toBigInteger()), (899998.toBigInteger()), (3778888999L.toBigInteger()), (277777788888899L.toBigInteger()))
    var i: Int = 0
    while (i < nums.size) {
        var n: BigInteger = nums[i]!!
        var r: MDRResult = multDigitalRoot(n, base)
        println((((pad(n.toString(), 20) + " ") + pad(r.mdr.toString(), 3)) + " ") + pad(r.mp.toString(), 3))
        i = i + 1
    }
    println("")
    var list: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var idx: Int = 0
    while (idx < base) {
        list = run { val _tmp = list.toMutableList(); _tmp.add(mutableListOf<Int>()); _tmp }
        idx = idx + 1
    }
    var cnt: BigInteger = (size * base).toBigInteger()
    var n: BigInteger = java.math.BigInteger.valueOf(0)
    var b: BigInteger = (base.toBigInteger())
    while (cnt.compareTo((0).toBigInteger()) > 0) {
        var r: MDRResult = multDigitalRoot(n, base)
        var mdr: Int = r.mdr
        if ((list[mdr]!!).size < size) {
            list[mdr] = run { val _tmp = (list[mdr]!!).toMutableList(); _tmp.add((n.toInt())); _tmp }
            cnt = cnt.subtract((1).toBigInteger())
        }
        n = n.add(((1.toBigInteger())))
    }
    println("MDR: First")
    var j: Int = 0
    while (j < base) {
        println((pad(j.toString(), 3) + ": ") + (list[j]!!).toString())
        j = j + 1
    }
}

fun main() {
    user_main()
}
