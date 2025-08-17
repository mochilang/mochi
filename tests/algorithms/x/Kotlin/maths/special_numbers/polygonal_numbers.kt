import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun polygonal_num(n: Int, sides: Int): Int {
    if ((n < 0) || (sides < 3)) {
        panic("Invalid input: num must be >= 0 and sides must be >= 3.")
    }
    var term1: Int = (((sides - 2) * n) * n).toInt()
    var term2: Int = ((sides - 4) * n).toInt()
    return (term1 - term2) / 2
}

fun user_main(): Unit {
    var n: Int = (5).toInt()
    var sides: Int = (4).toInt()
    var result: Int = (polygonal_num(n, sides)).toInt()
    println(_numToStr(result))
}

fun main() {
    user_main()
}
