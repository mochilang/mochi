import java.math.BigInteger

fun _len(v: Any?): Int = when (v) {
    is String -> v.length
    is Collection<*> -> v.size
    is Map<*, *> -> v.size
    else -> v.toString().length
}

fun sqrtApprox(x: Double): Double {
    var guess: Double = x
    var i: Int = 0
    while (i < 20) {
        guess = (guess + (x / guess)) / 2.0
        i = i + 1
    }
    return guess
}

fun makeSym(order: Int, elements: MutableList<Double>): MutableMap<String, Any?> {
    return mutableMapOf<String, Any?>("order" to (order), "ele" to (elements))
}

fun unpackSym(m: MutableMap<String, Any?>): MutableList<MutableList<Double>> {
    var n: Any? = (m)["order"] as Any?
    var ele: Any? = (m)["ele"] as Any?
    var mat: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var idx: Int = 0
    var r: Int = 0
    while ((r).toInt() < (n as Number).toDouble()) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = 0
        while (c <= r) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((ele as MutableList<Any?>)[idx]) as Double)); _tmp }
            idx = idx + 1
            c = c + 1
        }
        while ((c).toInt() < (n as Number).toDouble()) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            c = c + 1
        }
        mat = run { val _tmp = mat.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    r = 0
    while ((r).toInt() < (n as Number).toDouble()) {
        var c: BigInteger = (r + 1).toBigInteger()
        while (c.compareTo((n as Int).toBigInteger()) < 0) {
            (mat[r]!!)[(c).toInt()] = (((mat[(c).toInt()]!!) as MutableList<Double>))[r]!!
            c = c.add((1).toBigInteger())
        }
        r = r + 1
    }
    return mat
}

fun printMat(m: MutableList<MutableList<Double>>): Unit {
    var i: Int = 0
    while (i < m.size) {
        var line: String = ""
        var j: Int = 0
        while (j < (m[i]!!).size) {
            line = line + ((((m[i]!!) as MutableList<Double>))[j]!!).toString()
            if (j < ((m[i]!!).size - 1)) {
                line = line + " "
            }
            j = j + 1
        }
        println(line)
        i = i + 1
    }
}

fun printSym(m: MutableMap<String, Any?>): Unit {
    printMat(unpackSym(m))
}

fun printLower(m: MutableMap<String, Any?>): Unit {
    var n: Any? = (m)["order"] as Any?
    var ele: Any? = (m)["ele"] as Any?
    var mat: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var idx: Int = 0
    var r: Int = 0
    while ((r).toInt() < (n as Number).toDouble()) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var c: Int = 0
        while (c <= r) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((ele as MutableList<Any?>)[idx]) as Double)); _tmp }
            idx = idx + 1
            c = c + 1
        }
        while ((c).toInt() < (n as Number).toDouble()) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0.0); _tmp }
            c = c + 1
        }
        mat = run { val _tmp = mat.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    printMat(mat)
}

fun choleskyLower(a: MutableMap<String, Any?>): MutableMap<String, Any?> {
    var n: Any? = (a)["order"] as Any?
    var ae: Any? = (a)["ele"] as Any?
    var le: MutableList<Double> = mutableListOf<Double>()
    var idx: Int = 0
    while (idx < _len(ae)) {
        le = run { val _tmp = le.toMutableList(); _tmp.add(0.0); _tmp }
        idx = idx + 1
    }
    var row: Int = 1
    var col: Int = 1
    var dr: Int = 0
    var dc: Int = 0
    var i: Int = 0
    while (i < _len(ae)) {
        var e = (ae as MutableList<Any?>)[i]
        if (i < dr) {
            var d: Double = ((e as Number).toDouble() - le[i]!!) / le[dc]!!
            le[i] = d
            var ci: Int = col
            var cx: Int = dc
            var j: BigInteger = (i + 1).toBigInteger()
            while (j.compareTo((dr).toBigInteger()) <= 0) {
                cx = cx + ci
                ci = ci + 1
                le[(j).toInt()] = le[(j).toInt()]!! + (d * le[cx]!!)
                j = j.add((1).toBigInteger())
            }
            col = col + 1
            dc = dc + col
        } else {
            le[i] = sqrtApprox((e as Number).toDouble() - le[i]!!)
            row = row + 1
            dr = dr + row
            col = 1
            dc = 0
        }
        i = i + 1
    }
    return mutableMapOf<String, Any?>("order" to (n), "ele" to (le))
}

fun demo(a: MutableMap<String, Any?>): Unit {
    println("A:")
    printSym(a)
    println("L:")
    var l: MutableMap<String, Any?> = choleskyLower(a)
    printLower(l)
}

fun main() {
    demo(makeSym(3, mutableListOf(25.0, 15.0, 18.0, 0.0 - 5.0, 0.0, 11.0)))
    demo(makeSym(4, mutableListOf(18.0, 22.0, 70.0, 54.0, 86.0, 174.0, 42.0, 62.0, 134.0, 106.0)))
}
