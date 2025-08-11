import java.math.BigInteger

fun _sha256(data: Any?): MutableList<Int> {
val bytes = when (data) {
    is String -> data.toByteArray()
    is List<*> -> {
        val arr = ByteArray(data.size)
        for (i in data.indices) {
            val v = data[i]
            arr[i] = when (v) {
                is Number -> v.toInt().toByte()
                else -> 0
            }
        }
        arr
    }
    else -> ByteArray(0)
}
val md = java.security.MessageDigest.getInstance("SHA-256")
val hash = md.digest(bytes)
val res = mutableListOf<Int>()
for (b in hash) res.add(b.toInt() and 0xff)
return res
}

var HEX: String = "0123456789abcdef"
var expected: String = sha256_hex("233168")
var answer: String = solution_001()
var computed: String = sha256_hex(answer)
fun byte_to_hex(b: Int): String {
    var hi: Int = (b / 16).toInt()
    var lo: Int = (Math.floorMod(b, 16)).toInt()
    return HEX[hi].toString() + HEX[lo].toString()
}

fun bytes_to_hex(bs: MutableList<Int>): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < bs.size) {
        res = res + byte_to_hex(bs[i]!!)
        i = i + 1
    }
    return res
}

fun sha256_hex(s: String): String {
    return bytes_to_hex(_sha256(s))
}

fun solution_001(): String {
    var total: Int = (0).toInt()
    var n: Int = (0).toInt()
    while (n < 1000) {
        if (((Math.floorMod(n, 3)) == 0) || ((Math.floorMod(n, 5)) == 0)) {
            total = total + n
        }
        n = n + 1
    }
    return total.toString()
}

fun main() {
    if (computed == expected) {
        println("Problem 001 passed")
    } else {
        println((("Problem 001 failed: " + computed) + " != ") + expected)
    }
}
