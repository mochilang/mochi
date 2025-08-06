import java.math.BigInteger

var UPPER: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var LOWER: String = "abcdefghijklmnopqrstuvwxyz"
var BASE_TOP: String = "ABCDEFGHIJKLM"
var BASE_BOTTOM: String = "NOPQRSTUVWXYZ"
fun to_upper(s: String): String {
    var res: String = ""
    var i: Int = 0
    while (i < s.length) {
        var ch: String = s.substring(i, i + 1)
        var j: Int = 0
        var replaced: Boolean = false
        while (j < LOWER.length) {
            if (LOWER.substring(j, j + 1) == ch) {
                res = res + UPPER.substring(j, j + 1)
                replaced = true
                break
            }
            j = j + 1
        }
        if (!replaced) {
            res = res + ch
        }
        i = i + 1
    }
    return res
}

fun char_index(c: String): Int {
    var i: Int = 0
    while (i < UPPER.length) {
        if (UPPER.substring(i, i + 1) == c) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun rotate_right(s: String, k: Int): String {
    var n: Int = s.length
    var shift: Int = Math.floorMod(k, n)
    return s.substring(n - shift, n) + s.substring(0, n - shift)
}

fun table_for(c: String): MutableList<String> {
    var idx: Int = char_index(c)
    var shift: Int = idx / 2
    var row1: String = rotate_right(BASE_BOTTOM, shift)
    var pair: MutableList<String> = mutableListOf(BASE_TOP, row1)
    return pair
}

fun generate_table(key: String): MutableList<MutableList<String>> {
    var up: String = to_upper(key)
    var i: Int = 0
    var result: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    while (i < up.length) {
        var ch: String = up.substring(i, i + 1)
        var pair: MutableList<String> = table_for(ch)
        result = run { val _tmp = result.toMutableList(); _tmp.add(pair); _tmp }
        i = i + 1
    }
    return result
}

fun str_index(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun get_position(table: MutableList<String>, ch: String): MutableList<Int> {
    var row: Int = 0
    if (str_index(table[0]!!, ch) == (0 - 1)) {
        row = 1
    }
    var col: Int = str_index(table[row]!!, ch)
    return mutableListOf(row, col)
}

fun get_opponent(table: MutableList<String>, ch: String): String {
    var pos: MutableList<Int> = get_position(table, ch)
    var row: Int = pos[0]!!
    var col: Int = pos[1]!!
    if (col == (0 - 1)) {
        return ch
    }
    if (row == 1) {
        return table[0]!!.substring(col, col + 1)
    }
    return table[1]!!.substring(col, col + 1)
}

fun encrypt(key: String, words: String): String {
    var table: MutableList<MutableList<String>> = generate_table(key)
    var up_words: String = to_upper(words)
    var cipher: String = ""
    var count: Int = 0
    var i: Int = 0
    while (i < up_words.length) {
        var ch: String = up_words.substring(i, i + 1)
        cipher = cipher + get_opponent(table[count]!!, ch)
        count = Math.floorMod((count + 1), table.size)
        i = i + 1
    }
    return cipher
}

fun decrypt(key: String, words: String): String {
    var res: String = encrypt(key, words)
    return res
}

fun user_main(): Unit {
    println(encrypt("marvin", "jessica"))
    println(decrypt("marvin", "QRACRWU"))
}

fun main() {
    user_main()
}
