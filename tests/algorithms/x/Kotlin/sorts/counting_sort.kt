import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

var ascii_chars: String = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
var example1: MutableList<Int> = counting_sort(mutableListOf(0, 5, 3, 2, 2))
fun max_val(arr: MutableList<Int>): Int {
    var m: Int = (arr[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < arr.size) {
        if (arr[i]!! > m) {
            m = arr[i]!!
        }
        i = i + 1
    }
    return m
}

fun min_val(arr: MutableList<Int>): Int {
    var m: Int = (arr[0]!!).toInt()
    var i: Int = (1).toInt()
    while (i < arr.size) {
        if (arr[i]!! < m) {
            m = arr[i]!!
        }
        i = i + 1
    }
    return m
}

fun counting_sort(collection: MutableList<Int>): MutableList<Int> {
    if (collection.size == 0) {
        return mutableListOf<Int>()
    }
    var coll_len: Int = (collection.size).toInt()
    var coll_max: Int = (max_val(collection)).toInt()
    var coll_min: Int = (min_val(collection)).toInt()
    var counting_arr_length: Int = ((coll_max + 1) - coll_min).toInt()
    var counting_arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < counting_arr_length) {
        counting_arr = run { val _tmp = counting_arr.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    i = 0
    while (i < coll_len) {
        var number: Int = (collection[i]!!).toInt()
        _listSet(counting_arr, number - coll_min, counting_arr[number - coll_min]!! + 1)
        i = i + 1
    }
    i = 1
    while (i < counting_arr_length) {
        _listSet(counting_arr, i, counting_arr[i]!! + counting_arr[i - 1]!!)
        i = i + 1
    }
    var ordered: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < coll_len) {
        ordered = run { val _tmp = ordered.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var idx: BigInteger = ((coll_len - 1).toBigInteger())
    while (idx.compareTo((0).toBigInteger()) >= 0) {
        var number: Int = (collection[(idx).toInt()]!!).toInt()
        var pos: Int = (counting_arr[number - coll_min]!! - 1).toInt()
        _listSet(ordered, pos, number)
        _listSet(counting_arr, number - coll_min, counting_arr[number - coll_min]!! - 1)
        idx = idx.subtract((1).toBigInteger())
    }
    return ordered
}

fun chr(code: Int): String {
    if (code == 10) {
        return "\n"
    }
    if (code == 13) {
        return "\r"
    }
    if (code == 9) {
        return "\t"
    }
    if ((code >= 32) && (code < 127)) {
        return _sliceStr(ascii_chars, code - 32, code - 31)
    }
    return ""
}

fun ord(ch: String): Int {
    if (ch == "\n") {
        return 10
    }
    if (ch == "\r") {
        return 13
    }
    if (ch == "\t") {
        return 9
    }
    var i: Int = (0).toInt()
    while (i < ascii_chars.length) {
        if (_sliceStr(ascii_chars, i, i + 1) == ch) {
            return 32 + i
        }
        i = i + 1
    }
    return 0
}

fun counting_sort_string(s: String): String {
    var codes: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < s.length) {
        codes = run { val _tmp = codes.toMutableList(); _tmp.add(ord(_sliceStr(s, i, i + 1))); _tmp }
        i = i + 1
    }
    var sorted_codes: MutableList<Int> = counting_sort(codes)
    var res: String = ""
    i = 0
    while (i < sorted_codes.size) {
        res = res + chr(sorted_codes[i]!!)
        i = i + 1
    }
    return res
}

fun main() {
    println(example1.toString())
    var example2: MutableList<Int> = counting_sort(mutableListOf<Int>())
    println(example2.toString())
    var example3: MutableList<Int> = counting_sort(mutableListOf(0 - 2, 0 - 5, 0 - 45))
    println(example3.toString())
    println(counting_sort_string("thisisthestring"))
}
