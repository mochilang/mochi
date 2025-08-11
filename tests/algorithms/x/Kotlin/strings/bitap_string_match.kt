import java.math.BigInteger

fun pow2(n: Int): Int {
var v = 1
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun bit_and(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    var res: Int = (0).toInt()
    var bit: Int = (1).toInt()
    while ((x > 0) || (y > 0)) {
        if (((Math.floorMod(x, 2)) == 1) && ((Math.floorMod(y, 2)) == 1)) {
            res = res + bit
        }
        x = ((x / 2).toInt())
        y = ((y / 2).toInt())
        bit = bit * 2
    }
    return res
}

fun bit_or(a: Int, b: Int): Int {
    var x: Int = (a).toInt()
    var y: Int = (b).toInt()
    var res: Int = (0).toInt()
    var bit: Int = (1).toInt()
    while ((x > 0) || (y > 0)) {
        if (((Math.floorMod(x, 2)) == 1) || ((Math.floorMod(y, 2)) == 1)) {
            res = res + bit
        }
        x = ((x / 2).toInt())
        y = ((y / 2).toInt())
        bit = bit * 2
    }
    return res
}

fun char_to_index(ch: String): Int {
    var letters: String = "abcdefghijklmnopqrstuvwxyz"
    var i: Int = (0).toInt()
    while (i < letters.length) {
        if (_sliceStr(letters, i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 26
}

fun bitap_string_match(text: String, pattern: String): Int {
    if (pattern == "") {
        return 0
    }
    var m: Int = (pattern.length).toInt()
    if (m > text.length) {
        return 0 - 1
    }
    var limit: Int = (pow2(m + 1)).toInt()
    var all_ones: Int = (limit - 1).toInt()
    var pattern_mask: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < 27) {
        pattern_mask = run { val _tmp = pattern_mask.toMutableList(); _tmp.add(all_ones); _tmp }
        i = i + 1
    }
    i = 0
    while (i < m) {
        var ch: String = _sliceStr(pattern, i, i + 1)
        var idx: Int = (char_to_index(ch)).toInt()
        _listSet(pattern_mask, idx, bit_and(pattern_mask[idx]!!, all_ones - pow2(i)))
        i = i + 1
    }
    var state: BigInteger = ((all_ones - 1).toBigInteger())
    i = 0
    while (i < text.length) {
        var ch: String = _sliceStr(text, i, i + 1)
        var idx: Int = (char_to_index(ch)).toInt()
        state = ((bit_or((state.toInt()), pattern_mask[idx]!!)).toBigInteger())
        state = (state.multiply((2).toBigInteger())).remainder((limit).toBigInteger())
        if (bit_and((state.toInt()), pow2(m)) == 0) {
            return (i - m) + 1
        }
        i = i + 1
    }
    return 0 - 1
}

fun user_main(): Unit {
    println(bitap_string_match("abdabababc", "ababc").toString())
    println(bitap_string_match("abdabababc", "").toString())
    println(bitap_string_match("abdabababc", "c").toString())
    println(bitap_string_match("abdabababc", "fofosdfo").toString())
}

fun main() {
    user_main()
}
