import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var ETAOIN: String = "ETAOINSHRDLCUMWFGYPBVKJXQZ"
var LETTERS: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
fun etaoin_index(letter: String): Int {
    var i: Int = (0).toInt()
    while (i < ETAOIN.length) {
        if (ETAOIN.substring(i, i + 1) == letter) {
            return i
        }
        i = i + 1
    }
    return ETAOIN.length
}

fun get_letter_count(message: String): MutableMap<String, Int> {
    var letter_count: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var i: Int = (0).toInt()
    while (i < LETTERS.length) {
        var c: String = LETTERS.substring(i, i + 1)
        (letter_count)[c] = 0
        i = i + 1
    }
    var msg: String = (message.toUpperCase() as String)
    var j: Int = (0).toInt()
    while (j < msg.length) {
        var ch: String = msg.substring(j, j + 1)
        if (ch in LETTERS) {
            (letter_count)[ch] = (letter_count)[ch] as Int + 1
        }
        j = j + 1
    }
    return letter_count
}

fun get_frequency_order(message: String): String {
    var letter_to_freq: MutableMap<String, Int> = get_letter_count(message)
    var max_freq: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < LETTERS.length) {
        var letter: String = LETTERS.substring(i, i + 1)
        var f: Int = ((letter_to_freq)[letter] as Int).toInt()
        if (f > max_freq) {
            max_freq = f
        }
        i = i + 1
    }
    var result: String = ""
    var freq: Int = (max_freq).toInt()
    while (freq >= 0) {
        var group: MutableList<String> = mutableListOf<String>()
        var j: Int = (0).toInt()
        while (j < LETTERS.length) {
            var letter: String = LETTERS.substring(j, j + 1)
            if ((letter_to_freq)[letter] as Int == freq) {
                group = run { val _tmp = group.toMutableList(); _tmp.add(letter); _tmp }
            }
            j = j + 1
        }
        var g_len: Int = (group.size).toInt()
        var a: Int = (0).toInt()
        while (a < g_len) {
            var b: Int = (0).toInt()
            while (b < ((g_len - a) - 1)) {
                var g1: String = group[b]!!
                var g2: String = group[b + 1]!!
                var idx1: Int = (etaoin_index(g1)).toInt()
                var idx2: Int = (etaoin_index(g2)).toInt()
                if (idx1 < idx2) {
                    var tmp: String = group[b]!!
                    _listSet(group, b, group[b + 1]!!)
                    _listSet(group, b + 1, tmp)
                }
                b = b + 1
            }
            a = a + 1
        }
        var g: Int = (0).toInt()
        while (g < group.size) {
            result = result + group[g]!!
            g = g + 1
        }
        freq = freq - 1
    }
    return result
}

fun english_freq_match_score(message: String): Int {
    var freq_order: String = get_frequency_order(message)
    var top: String = freq_order.substring(0, 6)
    var bottom: String = freq_order.substring(freq_order.length - 6, freq_order.length)
    var score: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < 6) {
        var c: String = ETAOIN.substring(i, i + 1)
        if (c in top) {
            score = score + 1
        }
        i = i + 1
    }
    var j: BigInteger = ((ETAOIN.length - 6).toBigInteger())
    while (j.compareTo((ETAOIN.length).toBigInteger()) < 0) {
        var c: String = ETAOIN.substring((j).toInt(), (j.add((1).toBigInteger())).toInt())
        if (c in bottom) {
            score = score + 1
        }
        j = j.add((1).toBigInteger())
    }
    return score
}

fun user_main(): Unit {
    println(get_frequency_order("Hello World"))
    println(english_freq_match_score("Hello World"))
}

fun main() {
    user_main()
}
