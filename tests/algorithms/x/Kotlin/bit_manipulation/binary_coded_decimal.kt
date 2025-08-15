fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun to_binary4(n: Int): String {
    var result: String = ""
    var x: Int = (n).toInt()
    while (x > 0) {
        result = _numToStr(Math.floorMod(x, 2)) + result
        x = x / 2
    }
    while (result.length < 4) {
        result = "0" + result
    }
    return result
}

fun binary_coded_decimal(number: Int): String {
    var n: Int = (number).toInt()
    if (n < 0) {
        n = 0
    }
    var digits: String = _numToStr(n)
    var out: String = "0b"
    var i: Int = (0).toInt()
    while (i < digits.length) {
        var d: String = digits[i].toString()
        var d_int: Int = ((d.toBigInteger().toInt())).toInt()
        out = out + to_binary4(d_int)
        i = i + 1
    }
    return out
}

fun main() {
    println(binary_coded_decimal(0 - 2))
    println(binary_coded_decimal(0 - 1))
    println(binary_coded_decimal(0))
    println(binary_coded_decimal(3))
    println(binary_coded_decimal(2))
    println(binary_coded_decimal(12))
    println(binary_coded_decimal(987))
}
