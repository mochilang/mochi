import java.math.BigInteger

var seed: Int = (1).toInt()
fun set_seed(s: Int): Unit {
    seed = (s).toInt()
}

fun randint(a: Int, b: Int): Int {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt()).toInt()
    return (Math.floorMod(seed, ((b - a) + 1))) + a
}

fun jacobi_symbol(random_a: Int, number: Int): Int {
    var number: Int = (number).toInt()
    var random_a: Int = (random_a).toInt()
    if ((random_a == 0) || (random_a == 1)) {
        return random_a
    }
    random_a = Math.floorMod(random_a, number)
    var t: Int = (1).toInt()
    while (random_a != 0) {
        while ((Math.floorMod(random_a, 2)) == 0) {
            random_a = random_a / 2
            var r: Int = (Math.floorMod(number, 8)).toInt()
            if ((r == 3) || (r == 5)) {
                t = 0 - t
            }
        }
        var temp: Int = (random_a).toInt()
        random_a = number
        number = temp
        if (((Math.floorMod(random_a, 4)) == 3) && ((Math.floorMod(number, 4)) == 3)) {
            t = 0 - t
        }
        random_a = Math.floorMod(random_a, number)
    }
    if (number == 1) {
        return t
    }
    return 0
}

fun pow_mod(base: Int, exp: Int, mod: Int): Int {
    var result: Int = (1).toInt()
    var b: Int = (Math.floorMod(base, mod)).toInt()
    var e: Int = (exp).toInt()
    while (e > 0) {
        if ((Math.floorMod(e, 2)) == 1) {
            result = Math.floorMod((result * b), mod)
        }
        b = Math.floorMod((b * b), mod)
        e = e / 2
    }
    return result
}

fun solovay_strassen(number: Int, iterations: Int): Boolean {
    if (number <= 1) {
        return false
    }
    if (number <= 3) {
        return true
    }
    var i: Int = (0).toInt()
    while (i < iterations) {
        var a: Int = (randint(2, number - 2)).toInt()
        var x: Int = (jacobi_symbol(a, number)).toInt()
        var y: Int = (pow_mod(a, (number - 1) / 2, number)).toInt()
        var mod_x: Int = (Math.floorMod(x, number)).toInt()
        if (mod_x < 0) {
            mod_x = mod_x + number
        }
        if ((x == 0) || (y != mod_x)) {
            return false
        }
        i = i + 1
    }
    return true
}

fun user_main(): Unit {
    set_seed(10)
    println(solovay_strassen(13, 5).toString())
    println(solovay_strassen(9, 10).toString())
    println(solovay_strassen(17, 15).toString())
}

fun main() {
    user_main()
}
