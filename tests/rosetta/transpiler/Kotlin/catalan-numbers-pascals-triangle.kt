import java.math.BigInteger

var n: Int = 15
var t: MutableList<Int> = mutableListOf<Int>()
fun main() {
    for (_u1 in 0 until n + 2) {
        t = run { val _tmp = t.toMutableList(); _tmp.add(0); _tmp }
    }
    t[1] = 1
    for (i in 1 until n + 1) {
        var j: Int = i
        while (j > 1) {
            t[j] = t[j]!! + t[j - 1]!!
            j = j - 1
        }
        t[((i + 1).toInt())] = t[i]!!
        j = i + 1
        while (j > 1) {
            t[j] = t[j]!! + t[j - 1]!!
            j = j - 1
        }
        var cat: BigInteger = (t[i + 1]!! - t[i]!!).toBigInteger()
        if (i < 10) {
            println(((" " + i.toString()) + " : ") + cat.toString())
        } else {
            println((i.toString() + " : ") + cat.toString())
        }
    }
}
