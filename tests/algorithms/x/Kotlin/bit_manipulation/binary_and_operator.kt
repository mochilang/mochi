import java.math.BigInteger

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun to_binary(n: Int): String {
    if (n == 0) {
        return "0"
    }
    var num: Int = n
    var res: String = ""
    while (num > 0) {
        var bit: Int = Math.floorMod(num, 2)
        res = bit.toString() + res
        num = num / 2
    }
    return res
}

fun zfill(s: String, width: Int): String {
    var res: String = s
    var pad: BigInteger = ((width - s.length).toBigInteger())
    while (pad.compareTo((0).toBigInteger()) > 0) {
        res = "0" + res
        pad = pad.subtract((1).toBigInteger())
    }
    return res
}

fun binary_and(a: Int, b: Int): String {
    if ((a < 0) || (b < 0)) {
        panic("the value of both inputs must be positive")
    }
    var a_bin: String = to_binary(a)
    var b_bin: String = to_binary(b)
    var max_len: Int = a_bin.length
    if (b_bin.length > max_len) {
        max_len = b_bin.length
    }
    var a_pad: String = zfill(a_bin, max_len)
    var b_pad: String = zfill(b_bin, max_len)
    var i: Int = 0
    var res: String = ""
    while (i < max_len) {
        if ((a_pad[i].toString() == "1") && (b_pad[i].toString() == "1")) {
            res = res + "1"
        } else {
            res = res + "0"
        }
        i = i + 1
    }
    return "0b" + res
}

fun main() {
    println(binary_and(25, 32))
    println(binary_and(37, 50))
    println(binary_and(21, 30))
    println(binary_and(58, 73))
    println(binary_and(0, 255))
    println(binary_and(256, 256))
}
