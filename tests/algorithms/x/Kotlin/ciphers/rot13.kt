import java.math.BigInteger

var uppercase: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var lowercase: String = "abcdefghijklmnopqrstuvwxyz"
fun index_of(s: String, c: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == c) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun dencrypt(s: String, n: Int): String {
    var out: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        var idx_u: Int = index_of(uppercase, ch)
        if (idx_u >= 0) {
            var new_idx: Int = Math.floorMod((idx_u + n), 26)
            out = out + uppercase.substring(new_idx, new_idx + 1)
        } else {
            var idx_l: Int = index_of(lowercase, ch)
            if (idx_l >= 0) {
                var new_idx: Int = Math.floorMod((idx_l + n), 26)
                out = out + lowercase.substring(new_idx, new_idx + 1)
            } else {
                out = out + ch
            }
        }
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var msg: String = "My secret bank account number is 173-52946 so don't tell anyone!!"
    var s: String = dencrypt(msg, 13)
    println(s)
    println((dencrypt(s, 13) == msg).toString())
}

fun main() {
    user_main()
}
