import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var NEG_INF: Int = (0 - 1000000000).toInt()
var A: MutableList<Int> = mutableListOf(1, 2, 0 - 4, 7, 3, 0 - 5, 6, 11, 0 - 20, 9, 14, 15, 5, 2, 0 - 8)
var n: Int = (15).toInt()
var segment_tree: MutableList<Int> = init_int_array(n)
var lazy: MutableList<Int> = init_int_array(n)
var flag: MutableList<Boolean> = init_bool_array(n)
fun init_int_array(n: Int): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < ((4 * n) + 5)) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    return arr
}

fun init_bool_array(n: Int): MutableList<Boolean> {
    var arr: MutableList<Boolean> = mutableListOf<Boolean>()
    var i: Int = (0).toInt()
    while (i < ((4 * n) + 5)) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(false); _tmp }
        i = i + 1
    }
    return arr
}

fun left(idx: Int): Int {
    return idx * 2
}

fun right(idx: Int): Int {
    return (idx * 2) + 1
}

fun build(segment_tree: MutableList<Int>, idx: Int, l: Int, r: Int, a: MutableList<Int>): Unit {
    if (l == r) {
        _listSet(segment_tree, idx, a[l - 1]!!)
    } else {
        var mid: Int = ((l + r) / 2).toInt()
        build(segment_tree, left(idx), l, mid, a)
        build(segment_tree, right(idx), mid + 1, r, a)
        var lv: Int = (segment_tree[left(idx)]!!).toInt()
        var rv: Int = (segment_tree[right(idx)]!!).toInt()
        if (lv > rv) {
            _listSet(segment_tree, idx, lv)
        } else {
            _listSet(segment_tree, idx, rv)
        }
    }
}

fun update(segment_tree: MutableList<Int>, lazy: MutableList<Int>, flag: MutableList<Boolean>, idx: Int, l: Int, r: Int, a: Int, b: Int, _val: Int): Unit {
    if (((flag[idx]!!) as Boolean)) {
        _listSet(segment_tree, idx, lazy[idx]!!)
        _listSet(flag, idx, false)
        if (l != r) {
            _listSet(lazy, left(idx), lazy[idx]!!)
            _listSet(lazy, right(idx), lazy[idx]!!)
            _listSet(flag, left(idx), true)
            _listSet(flag, right(idx), true)
        }
    }
    if ((r < a) || (l > b)) {
        return
    }
    if ((l >= a) && (r <= b)) {
        _listSet(segment_tree, idx, _val)
        if (l != r) {
            _listSet(lazy, left(idx), _val)
            _listSet(lazy, right(idx), _val)
            _listSet(flag, left(idx), true)
            _listSet(flag, right(idx), true)
        }
        return
    }
    var mid: Int = ((l + r) / 2).toInt()
    update(segment_tree, lazy, flag, left(idx), l, mid, a, b, _val)
    update(segment_tree, lazy, flag, right(idx), mid + 1, r, a, b, _val)
    var lv: Int = (segment_tree[left(idx)]!!).toInt()
    var rv: Int = (segment_tree[right(idx)]!!).toInt()
    if (lv > rv) {
        _listSet(segment_tree, idx, lv)
    } else {
        _listSet(segment_tree, idx, rv)
    }
}

fun query(segment_tree: MutableList<Int>, lazy: MutableList<Int>, flag: MutableList<Boolean>, idx: Int, l: Int, r: Int, a: Int, b: Int): Int {
    if (((flag[idx]!!) as Boolean)) {
        _listSet(segment_tree, idx, lazy[idx]!!)
        _listSet(flag, idx, false)
        if (l != r) {
            _listSet(lazy, left(idx), lazy[idx]!!)
            _listSet(lazy, right(idx), lazy[idx]!!)
            _listSet(flag, left(idx), true)
            _listSet(flag, right(idx), true)
        }
    }
    if ((r < a) || (l > b)) {
        return NEG_INF
    }
    if ((l >= a) && (r <= b)) {
        return segment_tree[idx]!!
    }
    var mid: Int = ((l + r) / 2).toInt()
    var q1: Int = (query(segment_tree, lazy, flag, left(idx), l, mid, a, b)).toInt()
    var q2: Int = (query(segment_tree, lazy, flag, right(idx), mid + 1, r, a, b)).toInt()
    if (q1 > q2) {
        return q1
    } else {
        return q2
    }
}

fun segtree_to_string(segment_tree: MutableList<Int>, lazy: MutableList<Int>, flag: MutableList<Boolean>, n: Int): String {
    var res: String = "["
    var i: Int = (1).toInt()
    while (i <= n) {
        var v: Int = (query(segment_tree, lazy, flag, 1, 1, n, i, i)).toInt()
        res = res + v.toString()
        if (i < n) {
            res = res + ", "
        }
        i = i + 1
    }
    res = res + "]"
    return res
}

fun main() {
    build(segment_tree, 1, 1, n, A)
    println(query(segment_tree, lazy, flag, 1, 1, n, 4, 6))
    println(query(segment_tree, lazy, flag, 1, 1, n, 7, 11))
    println(query(segment_tree, lazy, flag, 1, 1, n, 7, 12))
    update(segment_tree, lazy, flag, 1, 1, n, 1, 3, 111)
    println(query(segment_tree, lazy, flag, 1, 1, n, 1, 15))
    update(segment_tree, lazy, flag, 1, 1, n, 7, 8, 235)
    println(segtree_to_string(segment_tree, lazy, flag, n))
}
