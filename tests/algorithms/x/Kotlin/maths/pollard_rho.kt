import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

data class PollardResult(var factor: Int = 0, var ok: Boolean = false)
fun gcd(a: Int, b: Int): Int {
    var x: Int = (if (a < 0) 0 - a else a.toInt()).toInt()
    var y: Int = (if (b < 0) 0 - b else b.toInt()).toInt()
    while (y != 0) {
        var t: Int = (Math.floorMod(x, y)).toInt()
        x = y
        y = t
    }
    return x
}

fun rand_fn(value: Int, step: Int, modulus: Int): Int {
    return (Math.floorMod((((value).toLong() * (value).toLong()) + (step).toLong()), (modulus).toLong())).toInt()
}

fun pollard_rho(num: Int, seed: Int, step: Int, attempts: Int): PollardResult {
    if (num < 2) {
        panic("The input value cannot be less than 2")
    }
    if ((num > 2) && ((Math.floorMod(num, 2)) == 0)) {
        return PollardResult(factor = 2, ok = true)
    }
    var s: Int = (seed).toInt()
    var st: Int = (step).toInt()
    var i: Int = (0).toInt()
    while (i < attempts) {
        var tortoise: Int = (s).toInt()
        var hare: Int = (s).toInt()
        while (true) {
            tortoise = rand_fn(tortoise, st, num)
            hare = rand_fn(hare, st, num)
            hare = rand_fn(hare, st, num)
            var divisor: Int = (gcd(hare - tortoise, num)).toInt()
            if (divisor == 1) {
                continue
            } else {
                if (divisor == num) {
                    break
                } else {
                    return PollardResult(factor = divisor, ok = true)
                }
            }
        }
        s = hare
        st = st + 1
        i = i + 1
    }
    return PollardResult(factor = 0, ok = false)
}

fun test_pollard_rho(): Unit {
    var r1: PollardResult = pollard_rho(8051, 2, 1, 5)
    if ((!r1.ok as Boolean) || (((r1.factor != 83) && (r1.factor != 97) as Boolean))) {
        panic("test1 failed")
    }
    var r2: PollardResult = pollard_rho(10403, 2, 1, 5)
    if ((!r2.ok as Boolean) || (((r2.factor != 101) && (r2.factor != 103) as Boolean))) {
        panic("test2 failed")
    }
    var r3: PollardResult = pollard_rho(100, 2, 1, 3)
    if ((!r3.ok as Boolean) || (r3.factor != 2)) {
        panic("test3 failed")
    }
    var r4: PollardResult = pollard_rho(17, 2, 1, 3)
    if ((r4.ok) as Boolean) {
        panic("test4 failed")
    }
    var r5: PollardResult = pollard_rho((17 * 17) * 17, 2, 1, 3)
    if ((!r5.ok as Boolean) || (r5.factor != 17)) {
        panic("test5 failed")
    }
    var r6: PollardResult = pollard_rho((17 * 17) * 17, 2, 1, 1)
    if ((r6.ok) as Boolean) {
        panic("test6 failed")
    }
    var r7: PollardResult = pollard_rho((3 * 5) * 7, 2, 1, 3)
    if ((!r7.ok as Boolean) || (r7.factor != 21)) {
        panic("test7 failed")
    }
}

fun user_main(): Unit {
    test_pollard_rho()
    var a: PollardResult = pollard_rho(100, 2, 1, 3)
    if ((a.ok) as Boolean) {
        println(_numToStr(a.factor))
    } else {
        println("None")
    }
    var b: PollardResult = pollard_rho(17, 2, 1, 3)
    if ((b.ok) as Boolean) {
        println(_numToStr(b.factor))
    } else {
        println("None")
    }
}

fun main() {
    user_main()
}
