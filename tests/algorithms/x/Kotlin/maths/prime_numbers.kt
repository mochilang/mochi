fun slow_primes(max_n: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (2).toInt()
    while (i <= max_n) {
        var j: Int = (2).toInt()
        var is_prime: Boolean = true
        while (j < i) {
            if ((Math.floorMod(i, j)) == 0) {
                is_prime = false
                break
            }
            j = j + 1
        }
        if (is_prime as Boolean) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(i); _tmp }
        }
        i = i + 1
    }
    return result
}

fun primes(max_n: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (2).toInt()
    while (i <= max_n) {
        var j: Int = (2).toInt()
        var is_prime: Boolean = true
        while (((j).toLong() * (j).toLong()) <= i) {
            if ((Math.floorMod(i, j)) == 0) {
                is_prime = false
                break
            }
            j = j + 1
        }
        if (is_prime as Boolean) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(i); _tmp }
        }
        i = i + 1
    }
    return result
}

fun fast_primes(max_n: Int): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    if (max_n >= 2) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(2); _tmp }
    }
    var i: Int = (3).toInt()
    while (i <= max_n) {
        var j: Int = (3).toInt()
        var is_prime: Boolean = true
        while (((j).toLong() * (j).toLong()) <= i) {
            if ((Math.floorMod(i, j)) == 0) {
                is_prime = false
                break
            }
            j = j + 2
        }
        if (is_prime as Boolean) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(i); _tmp }
        }
        i = i + 2
    }
    return result
}

fun main() {
    println(slow_primes(25).toString())
    println(primes(25).toString())
    println(fast_primes(25).toString())
}
