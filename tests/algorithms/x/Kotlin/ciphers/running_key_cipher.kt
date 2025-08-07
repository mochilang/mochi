import java.math.BigInteger

var key: String = "How does the duck know that? said Victor"
var plaintext: String = "DEFEND THIS"
var ciphertext: String = running_key_encrypt(key, plaintext)
fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun ord(ch: String): Int {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var idx: Int = upper.indexOf(ch)
    if (idx >= 0) {
        return 65 + idx
    }
    idx = lower.indexOf(ch)
    if (idx >= 0) {
        return 97 + idx
    }
    return 0
}

fun chr(n: Int): String {
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    if ((n >= 65) && (n < 91)) {
        return upper.substring(n - 65, n - 64)
    }
    if ((n >= 97) && (n < 123)) {
        return lower.substring(n - 97, n - 96)
    }
    return "?"
}

fun clean_text(s: String): String {
    var out: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s[i].toString()
        if ((ch >= "A") && (ch <= "Z")) {
            out = out + ch
        } else {
            if ((ch >= "a") && (ch <= "z")) {
                out = out + chr(ord(ch) - 32)
            }
        }
        i = i + 1
    }
    return out
}

fun running_key_encrypt(key: String, plaintext: String): String {
    var pt: String = clean_text(plaintext)
    var k: String = clean_text(key)
    var key_len: Int = k.length
    var res: String = ""
    var ord_a: Int = ord("A")
    var i: Int = 0
    while (i < pt.length) {
        var p: Int = ord(pt[i].toString()) - ord_a
        var kv: Int = ord(k[Math.floorMod(i, key_len)].toString()) - ord_a
        var c: Int = Math.floorMod((p + kv), 26)
        res = res + chr(c + ord_a)
        i = i + 1
    }
    return res
}

fun running_key_decrypt(key: String, ciphertext: String): String {
    var ct: String = clean_text(ciphertext)
    var k: String = clean_text(key)
    var key_len: Int = k.length
    var res: String = ""
    var ord_a: Int = ord("A")
    var i: Int = 0
    while (i < ct.length) {
        var c: Int = ord(ct[i].toString()) - ord_a
        var kv: Int = ord(k[Math.floorMod(i, key_len)].toString()) - ord_a
        var p: Int = Math.floorMod(((c - kv) + 26), 26)
        res = res + chr(p + ord_a)
        i = i + 1
    }
    return res
}

fun main() {
    println(ciphertext)
    println(running_key_decrypt(key, ciphertext))
}
