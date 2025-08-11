import java.math.BigInteger

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var DIGITS: String = "0123456789"
var LOWER: String = "abcdefghijklmnopqrstuvwxyz"
var UPPER: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
var example1: MutableList<String> = mutableListOf("2 ft 7 in", "1 ft 5 in", "10 ft 2 in", "2 ft 11 in", "7 ft 6 in")
fun index_of(s: String, ch: String): Int {
    var i: Int = (0).toInt()
    while (i < s.length) {
        if (s[i].toString() == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun is_digit(ch: String): Boolean {
    return index_of(DIGITS, ch) >= 0
}

fun to_lower(ch: String): String {
    var idx: Int = (index_of(UPPER, ch)).toInt()
    if (idx >= 0) {
        return _sliceStr(LOWER, idx, idx + 1)
    }
    return ch
}

fun pad_left(s: String, width: Int): String {
    var res: String = s
    while (res.length < width) {
        res = "0" + res
    }
    return res
}

fun alphanum_key(s: String): MutableList<String> {
    var key: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < s.length) {
        if (((is_digit(s[i].toString())) as Boolean)) {
            var num: String = ""
            while ((i < s.length) && is_digit(s[i].toString())) {
                num = num + s[i].toString()
                i = i + 1
            }
            var len_str: String = pad_left(num.length.toString(), 3)
            key = run { val _tmp = key.toMutableList(); _tmp.add(("#" + len_str) + num); _tmp }
        } else {
            var seg: String = ""
            while (i < s.length) {
                if (((is_digit(s[i].toString())) as Boolean)) {
                    break
                }
                seg = seg + to_lower(s[i].toString())
                i = i + 1
            }
            key = run { val _tmp = key.toMutableList(); _tmp.add(seg); _tmp }
        }
    }
    return key
}

fun compare_keys(a: MutableList<String>, b: MutableList<String>): Int {
    var i: Int = (0).toInt()
    while ((i < a.size) && (i < b.size)) {
        if (a[i]!! < b[i]!!) {
            return 0 - 1
        }
        if (a[i]!! > b[i]!!) {
            return 1
        }
        i = i + 1
    }
    if (a.size < b.size) {
        return 0 - 1
    }
    if (a.size > b.size) {
        return 1
    }
    return 0
}

fun natural_sort(arr: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var keys: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var k: Int = (0).toInt()
    while (k < arr.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(arr[k]!!); _tmp }
        keys = run { val _tmp = keys.toMutableList(); _tmp.add(alphanum_key(arr[k]!!)); _tmp }
        k = k + 1
    }
    var i: Int = (1).toInt()
    while (i < res.size) {
        var current: String = res[i]!!
        var current_key: MutableList<String> = keys[i]!!
        var j: BigInteger = ((i - 1).toBigInteger())
        while ((j.compareTo((0).toBigInteger()) >= 0) && (compare_keys(keys[(j).toInt()]!!, (current_key as MutableList<String>)) > 0)) {
            _listSet(res, (j.add((1).toBigInteger())).toInt(), res[(j).toInt()]!!)
            _listSet(keys, (j.add((1).toBigInteger())).toInt(), keys[(j).toInt()]!!)
            j = j.subtract((1).toBigInteger())
        }
        _listSet(res, (j.add((1).toBigInteger())).toInt(), current)
        _listSet(keys, (j.add((1).toBigInteger())).toInt(), (current_key as MutableList<String>))
        i = i + 1
    }
    return res
}

fun main() {
    println(natural_sort(example1).toString())
    var example2: MutableList<String> = mutableListOf("Elm11", "Elm12", "Elm2", "elm0", "elm1", "elm10", "elm13", "elm9")
    println(natural_sort(example2).toString())
}
