import java.math.BigInteger

var PI: Double = 3.141592653589793
fun conv2d(img: MutableList<MutableList<Double>>, k: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var h: Int = img.size
    var w: Int = (img[0]!!).size
    var n: Int = k.size
    var half: Int = n / 2
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var x: Int = 0
        while (x < w) {
            var sum: Double = 0.0
            var j: Int = 0
            while (j < n) {
                var i: Int = 0
                while (i < n) {
                    var yy: BigInteger = ((y + j) - half).toBigInteger()
                    if (yy.compareTo((0).toBigInteger()) < 0) {
                        yy = (0.toBigInteger())
                    }
                    if (yy.compareTo((h).toBigInteger()) >= 0) {
                        yy = ((h - 1).toBigInteger())
                    }
                    var xx: BigInteger = ((x + i) - half).toBigInteger()
                    if (xx.compareTo((0).toBigInteger()) < 0) {
                        xx = (0.toBigInteger())
                    }
                    if (xx.compareTo((w).toBigInteger()) >= 0) {
                        xx = ((w - 1).toBigInteger())
                    }
                    sum = sum + ((((img[(yy).toInt()]!!) as MutableList<Double>))[(xx).toInt()]!! * (((k[j]!!) as MutableList<Double>))[i]!!)
                    i = i + 1
                }
                j = j + 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sum); _tmp }
            x = x + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return out
}

fun gradient(img: MutableList<MutableList<Double>>): MutableList<MutableList<Double>> {
    var hx: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0 - 1.0, 0.0, 1.0), mutableListOf(0.0 - 2.0, 0.0, 2.0), mutableListOf(0.0 - 1.0, 0.0, 1.0))
    var hy: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(1.0, 2.0, 1.0), mutableListOf(0.0, 0.0, 0.0), mutableListOf(0.0 - 1.0, 0.0 - 2.0, 0.0 - 1.0))
    var gx: MutableList<MutableList<Double>> = conv2d(img, hx)
    var gy: MutableList<MutableList<Double>> = conv2d(img, hy)
    var h: Int = img.size
    var w: Int = (img[0]!!).size
    var out: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Double> = mutableListOf<Double>()
        var x: Int = 0
        while (x < w) {
            var g: Double = ((((gx[y]!!) as MutableList<Double>))[x]!! * (((gx[y]!!) as MutableList<Double>))[x]!!) + ((((gy[y]!!) as MutableList<Double>))[x]!! * (((gy[y]!!) as MutableList<Double>))[x]!!)
            row = run { val _tmp = row.toMutableList(); _tmp.add(g); _tmp }
            x = x + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return out
}

fun threshold(g: MutableList<MutableList<Double>>, t: Double): MutableList<MutableList<Int>> {
    var h: Int = g.size
    var w: Int = (g[0]!!).size
    var out: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var y: Int = 0
    while (y < h) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var x: Int = 0
        while (x < w) {
            if ((((g[y]!!) as MutableList<Double>))[x]!! >= t) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            }
            x = x + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(row); _tmp }
        y = y + 1
    }
    return out
}

fun printMatrix(m: MutableList<MutableList<Int>>): Unit {
    var y: Int = 0
    while (y < m.size) {
        var line: String = ""
        var x: Int = 0
        while (x < (m[0]!!).size) {
            line = line + ((((m[y]!!) as MutableList<Int>))[x]!!).toString()
            if (x < ((m[0]!!).size - 1)) {
                line = line + " "
            }
            x = x + 1
        }
        println(line)
        y = y + 1
    }
}

fun user_main(): Unit {
    var img: MutableList<MutableList<Double>> = mutableListOf(mutableListOf(0.0, 0.0, 0.0, 0.0, 0.0), mutableListOf(0.0, 255.0, 255.0, 255.0, 0.0), mutableListOf(0.0, 255.0, 255.0, 255.0, 0.0), mutableListOf(0.0, 255.0, 255.0, 255.0, 0.0), mutableListOf(0.0, 0.0, 0.0, 0.0, 0.0))
    var g: MutableList<MutableList<Double>> = gradient(img)
    var edges: MutableList<MutableList<Int>> = threshold(g, 1020.0 * 1020.0)
    printMatrix(edges)
}

fun main() {
    user_main()
}
