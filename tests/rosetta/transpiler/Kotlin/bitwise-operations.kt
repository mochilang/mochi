import java.math.BigInteger

fun toUnsigned16(n: Int): Int {
    var u: Int = n
    if (u < 0) {
        u = u + 65536
    }
    return Math.floorMod(u, 65536)
}

fun bin16(n: Int): String {
    var u: Int = toUnsigned16(n)
    var bits: String = ""
    var mask: Int = 32768
    for (i in 0 until 16) {
        if (u >= mask) {
            bits = bits + "1"
            u = u - mask
        } else {
            bits = bits + "0"
        }
        mask = ((mask / 2).toInt())
    }
    return bits
}

fun bit_and(a: Int, b: Int): Int {
    var ua: Int = toUnsigned16(a)
    var ub: Int = toUnsigned16(b)
    var res: Int = 0
    var bit: Int = 1
    for (i in 0 until 16) {
        if (((Math.floorMod(ua, 2)) == 1) && ((Math.floorMod(ub, 2)) == 1)) {
            res = res + bit
        }
        ua = ((ua / 2).toInt())
        ub = ((ub / 2).toInt())
        bit = bit * 2
    }
    return res
}

fun bit_or(a: Int, b: Int): Int {
    var ua: Int = toUnsigned16(a)
    var ub: Int = toUnsigned16(b)
    var res: Int = 0
    var bit: Int = 1
    for (i in 0 until 16) {
        if (((Math.floorMod(ua, 2)) == 1) || ((Math.floorMod(ub, 2)) == 1)) {
            res = res + bit
        }
        ua = ((ua / 2).toInt())
        ub = ((ub / 2).toInt())
        bit = bit * 2
    }
    return res
}

fun bit_xor(a: Int, b: Int): Int {
    var ua: Int = toUnsigned16(a)
    var ub: Int = toUnsigned16(b)
    var res: Int = 0
    var bit: Int = 1
    for (i in 0 until 16) {
        var abit: BigInteger = (Math.floorMod(ua, 2)).toBigInteger()
        var bbit: BigInteger = (Math.floorMod(ub, 2)).toBigInteger()
        if ((((abit.compareTo((1).toBigInteger()) == 0) && (bbit.compareTo((0).toBigInteger()) == 0) as Boolean)) || (((abit.compareTo((0).toBigInteger()) == 0) && (bbit.compareTo((1).toBigInteger()) == 0) as Boolean))) {
            res = res + bit
        }
        ua = ((ua / 2).toInt())
        ub = ((ub / 2).toInt())
        bit = bit * 2
    }
    return res
}

fun bit_not(a: Int): Int {
    var ua: Int = toUnsigned16(a)
    return 65535 - ua
}

fun shl(a: Int, b: Int): Int {
    var ua: Int = toUnsigned16(a)
    var i: Int = 0
    while (i < b) {
        ua = Math.floorMod((ua * 2), 65536)
        i = i + 1
    }
    return ua
}

fun shr(a: Int, b: Int): Int {
    var ua: Int = toUnsigned16(a)
    var i: Int = 0
    while (i < b) {
        ua = ((ua / 2).toInt())
        i = i + 1
    }
    return ua
}

fun las(a: Int, b: Int): Int {
    return shl(a, b)
}

fun ras(a: Int, b: Int): Int {
    var _val: Int = a
    var i: Int = 0
    while (i < b) {
        if (_val >= 0) {
            _val = ((_val / 2).toInt())
        } else {
            _val = (((_val - 1) / 2).toInt())
        }
        i = i + 1
    }
    return toUnsigned16(_val)
}

fun rol(a: Int, b: Int): Int {
    var ua: Int = toUnsigned16(a)
    var left: Int = shl(ua, b)
    var right: Int = shr(ua, 16 - b)
    return toUnsigned16(left + right)
}

fun ror(a: Int, b: Int): Int {
    var ua: Int = toUnsigned16(a)
    var right: Int = shr(ua, b)
    var left: Int = shl(ua, 16 - b)
    return toUnsigned16(left + right)
}

fun bitwise(a: Int, b: Int): Unit {
    println("a:   " + bin16(a))
    println("b:   " + bin16(b))
    println("and: " + bin16(bit_and(a, b)))
    println("or:  " + bin16(bit_or(a, b)))
    println("xor: " + bin16(bit_xor(a, b)))
    println("not: " + bin16(bit_not(a)))
    if (b < 0) {
        println("Right operand is negative, but all shifts require an unsigned right operand (shift distance).")
        return
    }
    println("shl: " + bin16(shl(a, b)))
    println("shr: " + bin16(shr(a, b)))
    println("las: " + bin16(las(a, b)))
    println("ras: " + bin16(ras(a, b)))
    println("rol: " + bin16(rol(a, b)))
    println("ror: " + bin16(ror(a, b)))
}

fun main() {
    bitwise(0 - 460, 6)
}
