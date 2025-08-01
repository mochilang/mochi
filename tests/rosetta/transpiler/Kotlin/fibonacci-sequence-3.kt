import java.math.BigInteger

fun fibNumber(): () -> Int {
    var a: Int = 0
    var b: Int = 1
    return {
    var tmp: BigInteger = (a + b).toBigInteger()
    a = b
    b = tmp.toInt()
    a
} as () -> Int
}

fun fibSequence(n: Int): Int {
    var f: () -> Int = fibNumber()
    var r: Int = 0
    var i: Int = 0
    while (i < n) {
        r = (f()).toInt()
        i = i + 1
    }
    return r
}

fun main() {
}
