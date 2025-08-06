import java.math.BigInteger

var seed: Int = 1
var ascii_chars: String = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
fun set_seed(s: Int): Unit {
    seed = s
}

fun randint(a: Int, b: Int): Int {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())
    return (Math.floorMod(seed, ((b - a) + 1))) + a
}

fun ord(ch: String): Int {
    var i: Int = 0
    while (i < ascii_chars.length) {
        if (ascii_chars[i].toString() == ch) {
            return 32 + i
        }
        i = i + 1
    }
    return 0
}

fun chr(code: Int): String {
    if ((code < 32) || (code > 126)) {
        return ""
    }
    return ascii_chars[code - 32].toString()
}

fun encrypt(text: String): MutableMap<String, MutableList<Int>> {
    var cipher: MutableList<Int> = mutableListOf<Int>()
    var key: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < text.length) {
        var p: Int = ord(text[i].toString())
        var k: Int = randint(1, 300)
        var c: Int = (p + k) * k
        cipher = run { val _tmp = cipher.toMutableList(); _tmp.add(c); _tmp }
        key = run { val _tmp = key.toMutableList(); _tmp.add(k); _tmp }
        i = i + 1
    }
    var res: MutableMap<String, MutableList<Int>> = mutableMapOf<String, MutableList<Int>>()
    (res)["cipher"] = cipher
    (res)["key"] = key
    return res
}

fun decrypt(cipher: MutableList<Int>, key: MutableList<Int>): String {
    var plain: String = ""
    var i: Int = 0
    while (i < key.size) {
        var p: Int = (cipher[i]!! - (key[i]!! * key[i]!!)) / key[i]!!
        plain = plain + chr(p)
        i = i + 1
    }
    return plain
}

fun main() {
    set_seed(1)
    var res: MutableMap<String, MutableList<Int>> = encrypt("Hello")
    var cipher: MutableList<Int> = (res)["cipher"] as MutableList<Int>
    var key: MutableList<Int> = (res)["key"] as MutableList<Int>
    println(cipher)
    println(key)
    println(decrypt(cipher, key))
}
