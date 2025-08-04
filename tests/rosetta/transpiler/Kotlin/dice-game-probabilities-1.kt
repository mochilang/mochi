import java.math.BigInteger

fun powInt(base: Int, exp: Int): Int {
    var r: Int = 1
    var b: Int = base
    var e: Int = exp
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            r = r * b
        }
        b = b * b
        e = e / ((2.toInt()))
    }
    return r
}

fun minInt(x: Int, y: Int): Int {
    if (x < y) {
        return x
    }
    return y
}

fun throwDie(nSides: Int, nDice: Int, s: Int, counts: MutableList<Int>): Unit {
    if (nDice == 0) {
        counts[s] = counts[s]!! + 1
        return
    }
    var i: Int = 1
    while (i <= nSides) {
        throwDie(nSides, nDice - 1, s + i, counts)
        i = i + 1
    }
}

fun beatingProbability(nSides1: Int, nDice1: Int, nSides2: Int, nDice2: Int): Double {
    var len1: BigInteger = ((nSides1 + 1) * nDice1).toBigInteger()
    var c1: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while ((i).toBigInteger().compareTo((len1)) < 0) {
        c1 = run { val _tmp = c1.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    throwDie(nSides1, nDice1, 0, c1)
    var len2: BigInteger = ((nSides2 + 1) * nDice2).toBigInteger()
    var c2: MutableList<Int> = mutableListOf<Int>()
    var j: Int = 0
    while ((j).toBigInteger().compareTo((len2)) < 0) {
        c2 = run { val _tmp = c2.toMutableList(); _tmp.add(0); _tmp }
        j = j + 1
    }
    throwDie(nSides2, nDice2, 0, c2)
    var p12: Double = (((powInt(nSides1, nDice1)).toDouble())) * (((powInt(nSides2, nDice2)).toDouble()))
    var tot: Double = 0.0
    i = 0
    while ((i).toBigInteger().compareTo((len1)) < 0) {
        j = 0
        var m: Int = minInt(i, (len2.toInt()))
        while (j < m) {
            tot = tot + ((c1[i]!! * (((c2[j]!!).toDouble()))) / p12)
            j = j + 1
        }
        i = i + 1
    }
    return tot
}

fun main() {
    println(beatingProbability(4, 9, 6, 6).toString())
    println(beatingProbability(10, 5, 7, 6).toString())
}
