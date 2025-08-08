import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class FenwickTree(var size: Int = 0, var tree: MutableList<Int> = mutableListOf<Int>())
var f_base: FenwickTree = fenwick_from_list(mutableListOf(1, 2, 3, 4, 5))
fun fenwick_from_list(arr: MutableList<Int>): FenwickTree {
    var size: Int = (arr.size).toInt()
    var tree: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < size) {
        tree = run { val _tmp = tree.toMutableList(); _tmp.add(arr[i]!!); _tmp }
        i = i + 1
    }
    i = 1
    while (i < size) {
        var j: Int = (fenwick_next(i)).toInt()
        if (j < size) {
            _listSet(tree, j, tree[j]!! + tree[i]!!)
        }
        i = i + 1
    }
    return FenwickTree(size = size, tree = tree)
}

fun fenwick_empty(size: Int): FenwickTree {
    var tree: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < size) {
        tree = run { val _tmp = tree.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    return FenwickTree(size = size, tree = tree)
}

fun fenwick_get_array(f: FenwickTree): MutableList<Int> {
    var arr: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < f.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add((f.tree)[i]!!); _tmp }
        i = i + 1
    }
    i = f.size - 1
    while (i > 0) {
        var j: Int = (fenwick_next(i)).toInt()
        if (j < f.size) {
            _listSet(arr, j, arr[j]!! - arr[i]!!)
        }
        i = i - 1
    }
    return arr
}

fun bit_and(a: Int, b: Int): Int {
    var ua: Int = (a).toInt()
    var ub: Int = (b).toInt()
    var res: Int = (0).toInt()
    var bit: Int = (1).toInt()
    while ((ua != 0) || (ub != 0)) {
        if (((Math.floorMod(ua, 2)) == 1) && ((Math.floorMod(ub, 2)) == 1)) {
            res = res + bit
        }
        ua = ((ua / 2).toInt())
        ub = ((ub / 2).toInt())
        bit = bit * 2
    }
    return res
}

fun low_bit(x: Int): Int {
    if (x == 0) {
        return 0
    }
    return x - bit_and(x, x - 1)
}

fun fenwick_next(index: Int): Int {
    return index + low_bit(index)
}

fun fenwick_prev(index: Int): Int {
    return index - low_bit(index)
}

fun fenwick_add(f: FenwickTree, index: Int, value: Int): FenwickTree {
    var tree: MutableList<Int> = f.tree
    if (index == 0) {
        _listSet(tree, 0, tree[0]!! + value)
        return FenwickTree(size = f.size, tree = tree)
    }
    var i: Int = (index).toInt()
    while (i < f.size) {
        _listSet(tree, i, tree[i]!! + value)
        i = fenwick_next(i)
    }
    return FenwickTree(size = f.size, tree = tree)
}

fun fenwick_update(f: FenwickTree, index: Int, value: Int): FenwickTree {
    var current: Int = (fenwick_get(f, index)).toInt()
    return fenwick_add(f, index, value - current)
}

fun fenwick_prefix(f: FenwickTree, right: Int): Int {
    if (right == 0) {
        return 0
    }
    var result: Int = ((f.tree)[0]!!).toInt()
    var r: BigInteger = ((right - 1).toBigInteger())
    while (r.compareTo((0).toBigInteger()) > 0) {
        result = result + (f.tree)[(r).toInt()]!!
        r = ((fenwick_prev((r.toInt()))).toBigInteger())
    }
    return result
}

fun fenwick_query(f: FenwickTree, left: Int, right: Int): Int {
    return fenwick_prefix(f, right) - fenwick_prefix(f, left)
}

fun fenwick_get(f: FenwickTree, index: Int): Int {
    return fenwick_query(f, index, index + 1)
}

fun fenwick_rank_query(f: FenwickTree, value: Int): Int {
    var v: BigInteger = ((value - (f.tree)[0]!!).toBigInteger())
    if (v.compareTo((0).toBigInteger()) < 0) {
        return 0 - 1
    }
    var j: Int = (1).toInt()
    while ((j * 2) < f.size) {
        j = j * 2
    }
    var i: Int = (0).toInt()
    var jj: Int = (j).toInt()
    while (jj > 0) {
        if (((i + jj) < f.size) && (((f.tree)[i + jj]!!).toBigInteger().compareTo((v)) <= 0)) {
            v = v.subtract(((f.tree)[i + jj]!!).toBigInteger())
            i = i + jj
        }
        jj = jj / 2
    }
    return i
}

fun main() {
    println(fenwick_get_array(f_base))
    var f: FenwickTree = fenwick_from_list(mutableListOf(1, 2, 3, 4, 5))
    f = fenwick_add(f, 0, 1)
    f = fenwick_add(f, 1, 2)
    f = fenwick_add(f, 2, 3)
    f = fenwick_add(f, 3, 4)
    f = fenwick_add(f, 4, 5)
    println(fenwick_get_array(f))
    var f2: FenwickTree = fenwick_from_list(mutableListOf(1, 2, 3, 4, 5))
    println(fenwick_prefix(f2, 3))
    println(fenwick_query(f2, 1, 4))
    var f3: FenwickTree = fenwick_from_list(mutableListOf(1, 2, 0, 3, 0, 5))
    println(fenwick_rank_query(f3, 0))
    println(fenwick_rank_query(f3, 2))
    println(fenwick_rank_query(f3, 1))
    println(fenwick_rank_query(f3, 3))
    println(fenwick_rank_query(f3, 5))
    println(fenwick_rank_query(f3, 6))
    println(fenwick_rank_query(f3, 11))
}
