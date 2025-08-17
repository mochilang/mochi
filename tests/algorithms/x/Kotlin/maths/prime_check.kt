fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun is_prime(number: Int): Boolean {
    if (number < 0) {
        panic("is_prime() only accepts positive integers")
    }
    if ((1 < number) && (number < 4)) {
        return true
    } else {
        if ((((number < 2) || ((Math.floorMod(number, 2)) == 0) as Boolean)) || ((Math.floorMod(number, 3)) == 0)) {
            return false
        }
    }
    var i: Int = (5).toInt()
    while (((i).toLong() * (i).toLong()) <= number) {
        if (((Math.floorMod(number, i)) == 0) || ((Math.floorMod(number, (i + 2))) == 0)) {
            return false
        }
        i = i + 6
    }
    return true
}

fun main() {
    println(is_prime(0).toString())
    println(is_prime(1).toString())
    println(is_prime(2).toString())
    println(is_prime(3).toString())
    println(is_prime(27).toString())
    println(is_prime(87).toString())
    println(is_prime(563).toString())
    println(is_prime(2999).toString())
    println(is_prime(67483).toString())
}
