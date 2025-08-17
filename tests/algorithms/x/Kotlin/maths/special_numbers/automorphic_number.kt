import java.math.BigInteger

fun is_automorphic_number(number: Int): Boolean {
    if (number < 0) {
        return false
    }
    var n: Int = (number).toInt()
    var sq: Int = (number * number).toInt()
    while (n > 0) {
        if ((Math.floorMod(n, 10)) != (Math.floorMod(sq, 10))) {
            return false
        }
        n = n / 10
        sq = sq / 10
    }
    return true
}

fun main() {
    println(is_automorphic_number(0).toString())
    println(is_automorphic_number(1).toString())
    println(is_automorphic_number(5).toString())
    println(is_automorphic_number(6).toString())
    println(is_automorphic_number(7).toString())
    println(is_automorphic_number(25).toString())
    println(is_automorphic_number(376).toString())
}
