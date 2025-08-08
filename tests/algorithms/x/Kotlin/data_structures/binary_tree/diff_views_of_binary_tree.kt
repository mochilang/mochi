import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class Tree(var values: MutableList<Int> = mutableListOf<Int>(), var lefts: MutableList<Int> = mutableListOf<Int>(), var rights: MutableList<Int> = mutableListOf<Int>(), var root: Int = 0)
data class Pair(var idx: Int = 0, var hd: Int = 0)
var NIL: Int = (0 - 1).toInt()
var tree: Tree = make_tree()
fun make_tree(): Tree {
    return Tree(values = mutableListOf(3, 9, 20, 15, 7), lefts = mutableListOf<Any?>((1 as Any?), (NIL as Any?), (3 as Any?), (NIL as Any?), (NIL as Any?)), rights = mutableListOf<Any?>((2 as Any?), (NIL as Any?), (4 as Any?), (NIL as Any?), (NIL as Any?)), root = 0)
}

fun index_of(xs: MutableList<Int>, x: Int): Int {
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return i
        }
        i = i + 1
    }
    return (NIL.toInt())
}

fun sort_pairs(hds: MutableList<Int>, vals: MutableList<Int>): Unit {
    var i: Int = (0).toInt()
    while (i < hds.size) {
        var j: Int = (i).toInt()
        while ((j > 0) && (hds[j - 1]!! > hds[j]!!)) {
            var hd_tmp: Int = (hds[j - 1]!!).toInt()
            _listSet(hds, j - 1, hds[j]!!)
            _listSet(hds, j, hd_tmp)
            var val_tmp: Int = (vals[j - 1]!!).toInt()
            _listSet(vals, j - 1, vals[j]!!)
            _listSet(vals, j, val_tmp)
            j = j - 1
        }
        i = i + 1
    }
}

fun right_view(t: Tree): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var queue: MutableList<Int> = mutableListOf(t.root)
    while (queue.size > 0) {
        var size: Int = (queue.size).toInt()
        var i: Int = (0).toInt()
        while (i < size) {
            var idx: Int = (queue[i]!!).toInt()
            if (((t.lefts)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
                queue = run { val _tmp = queue.toMutableList(); _tmp.add((t.lefts)[idx]!!); _tmp }
            }
            if (((t.rights)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
                queue = run { val _tmp = queue.toMutableList(); _tmp.add((t.rights)[idx]!!); _tmp }
            }
            i = i + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add((t.values)[queue[size - 1]!!]!!); _tmp }
        queue = queue.subList(size, queue.size)
    }
    return res
}

fun left_view(t: Tree): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var queue: MutableList<Int> = mutableListOf(t.root)
    while (queue.size > 0) {
        var size: Int = (queue.size).toInt()
        var i: Int = (0).toInt()
        while (i < size) {
            var idx: Int = (queue[i]!!).toInt()
            if (((t.lefts)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
                queue = run { val _tmp = queue.toMutableList(); _tmp.add((t.lefts)[idx]!!); _tmp }
            }
            if (((t.rights)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
                queue = run { val _tmp = queue.toMutableList(); _tmp.add((t.rights)[idx]!!); _tmp }
            }
            i = i + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add((t.values)[queue[0]!!]!!); _tmp }
        queue = queue.subList(size, queue.size)
    }
    return res
}

fun top_view(t: Tree): MutableList<Int> {
    var hds: MutableList<Int> = mutableListOf<Int>()
    var vals: MutableList<Int> = mutableListOf<Int>()
    var queue_idx: MutableList<Int> = mutableListOf(t.root)
    var queue_hd: MutableList<Int> = mutableListOf(0)
    while (queue_idx.size > 0) {
        var idx: Int = (queue_idx[0]!!).toInt()
        queue_idx = queue_idx.subList(1, queue_idx.size)
        var hd: Int = (queue_hd[0]!!).toInt()
        queue_hd = queue_hd.subList(1, queue_hd.size)
        if ((index_of(hds, hd)).toBigInteger().compareTo((NIL)) == 0) {
            hds = run { val _tmp = hds.toMutableList(); _tmp.add(hd); _tmp }
            vals = run { val _tmp = vals.toMutableList(); _tmp.add((t.values)[idx]!!); _tmp }
        }
        if (((t.lefts)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
            queue_idx = run { val _tmp = queue_idx.toMutableList(); _tmp.add((t.lefts)[idx]!!); _tmp }
            queue_hd = run { val _tmp = queue_hd.toMutableList(); _tmp.add(hd - 1); _tmp }
        }
        if (((t.rights)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
            queue_idx = run { val _tmp = queue_idx.toMutableList(); _tmp.add((t.rights)[idx]!!); _tmp }
            queue_hd = run { val _tmp = queue_hd.toMutableList(); _tmp.add(hd + 1); _tmp }
        }
    }
    sort_pairs(hds, vals)
    return vals
}

fun bottom_view(t: Tree): MutableList<Int> {
    var hds: MutableList<Int> = mutableListOf<Int>()
    var vals: MutableList<Int> = mutableListOf<Int>()
    var queue_idx: MutableList<Int> = mutableListOf(t.root)
    var queue_hd: MutableList<Int> = mutableListOf(0)
    while (queue_idx.size > 0) {
        var idx: Int = (queue_idx[0]!!).toInt()
        queue_idx = queue_idx.subList(1, queue_idx.size)
        var hd: Int = (queue_hd[0]!!).toInt()
        queue_hd = queue_hd.subList(1, queue_hd.size)
        var pos: Int = (index_of(hds, hd)).toInt()
        if ((pos).toBigInteger().compareTo((NIL)) == 0) {
            hds = run { val _tmp = hds.toMutableList(); _tmp.add(hd); _tmp }
            vals = run { val _tmp = vals.toMutableList(); _tmp.add((t.values)[idx]!!); _tmp }
        } else {
            _listSet(vals, pos, (t.values)[idx]!!)
        }
        if (((t.lefts)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
            queue_idx = run { val _tmp = queue_idx.toMutableList(); _tmp.add((t.lefts)[idx]!!); _tmp }
            queue_hd = run { val _tmp = queue_hd.toMutableList(); _tmp.add(hd - 1); _tmp }
        }
        if (((t.rights)[idx]!!).toBigInteger().compareTo((NIL)) != 0) {
            queue_idx = run { val _tmp = queue_idx.toMutableList(); _tmp.add((t.rights)[idx]!!); _tmp }
            queue_hd = run { val _tmp = queue_hd.toMutableList(); _tmp.add(hd + 1); _tmp }
        }
    }
    sort_pairs(hds, vals)
    return vals
}

fun main() {
    println(right_view(tree))
    println(left_view(tree))
    println(top_view(tree))
    println(bottom_view(tree))
}
