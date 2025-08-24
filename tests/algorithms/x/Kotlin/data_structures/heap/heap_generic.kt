import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/heap"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

data class Heap(var arr: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>(), var pos_map: MutableMap<Int, Int> = mutableMapOf<Int, Int>(), var size: Int = 0, var key: (Int) -> Int)
var h: Heap = new_heap(::identity)
fun new_heap(key: (Int) -> Int): Heap {
    return Heap(arr = mutableListOf<MutableList<Int>>(), pos_map = mutableMapOf<Int, Int>(), size = 0, key = key)
}

fun parent(i: Int): Int {
    if (i > 0) {
        return (i - 1) / 2
    }
    return 0 - 1
}

fun left(i: Int, size: Int): Int {
    var l: Int = ((2 * i) + 1).toInt()
    if (l < size) {
        return l
    }
    return 0 - 1
}

fun right(i: Int, size: Int): Int {
    var r: Int = ((2 * i) + 2).toInt()
    if (r < size) {
        return r
    }
    return 0 - 1
}

fun swap(h: Heap, i: Int, j: Int): Unit {
    var arr: MutableList<MutableList<Int>> = h.arr
    var item_i: Int = (((arr[i]!!) as MutableList<Int>)[0]!!).toInt()
    var item_j: Int = (((arr[j]!!) as MutableList<Int>)[0]!!).toInt()
    var pm: MutableMap<Int, Int> = h.pos_map
    (pm)[item_i] = j + 1
    (pm)[item_j] = i + 1
    h.pos_map = pm
    var tmp: MutableList<Int> = arr[i]!!
    _listSet(arr, i, arr[j]!!)
    _listSet(arr, j, tmp)
    h.arr = arr
}

fun cmp(h: Heap, i: Int, j: Int): Boolean {
    var arr: MutableList<MutableList<Int>> = h.arr
    return ((arr[i]!!) as MutableList<Int>)[1]!! < ((arr[j]!!) as MutableList<Int>)[1]!!
}

fun get_valid_parent(h: Heap, i: Int): Int {
    var vp: Int = (i).toInt()
    var l: Int = (left(i, h.size)).toInt()
    if ((l != (0 - 1)) && (cmp(h, l, vp) == false)) {
        vp = l
    }
    var r: Int = (right(i, h.size)).toInt()
    if ((r != (0 - 1)) && (cmp(h, r, vp) == false)) {
        vp = r
    }
    return vp
}

fun heapify_up(h: Heap, index: Int): Unit {
    var idx: Int = (index).toInt()
    var p: Int = (parent(idx)).toInt()
    while ((p != (0 - 1)) && (cmp(h, idx, p) == false)) {
        swap(h, idx, p)
        idx = p
        p = parent(p)
    }
}

fun heapify_down(h: Heap, index: Int): Unit {
    var idx: Int = (index).toInt()
    var vp: Int = (get_valid_parent(h, idx)).toInt()
    while (vp != idx) {
        swap(h, idx, vp)
        idx = vp
        vp = get_valid_parent(h, idx)
    }
}

fun update_item(h: Heap, item: Int, item_value: Int): Unit {
    var pm: MutableMap<Int, Int> = h.pos_map
    if ((pm)[item] as Int == 0) {
        return
    }
    var index: Int = ((pm)[item] as Int - 1).toInt()
    var arr: MutableList<MutableList<Int>> = h.arr
    _listSet(arr, index, mutableListOf<Any?>(item as Any?, h.key(item_value)) as MutableList<Int>)
    h.arr = arr
    h.pos_map = pm
    heapify_up(h, index)
    heapify_down(h, index)
}

fun delete_item(h: Heap, item: Int): Unit {
    var pm: MutableMap<Int, Int> = h.pos_map
    if ((pm)[item] as Int == 0) {
        return
    }
    var index: Int = ((pm)[item] as Int - 1).toInt()
    (pm)[item] = 0
    var arr: MutableList<MutableList<Int>> = h.arr
    var last_index: Int = (h.size - 1).toInt()
    if (index != last_index) {
        _listSet(arr, index, arr[last_index]!!)
        var moved: Int = (((arr[index]!!) as MutableList<Int>)[0]!!).toInt()
        (pm)[moved] = index + 1
    }
    h.size = h.size - 1
    h.arr = arr
    h.pos_map = pm
    if (h.size > index) {
        heapify_up(h, index)
        heapify_down(h, index)
    }
}

fun insert_item(h: Heap, item: Int, item_value: Int): Unit {
    var arr: MutableList<MutableList<Int>> = h.arr
    var arr_len: Int = (arr.size).toInt()
    if (arr_len == h.size) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(mutableListOf<Any?>(item as Any?, h.key(item_value)) as MutableList<Int>); _tmp }
    } else {
        _listSet(arr, h.size, mutableListOf<Any?>(item as Any?, h.key(item_value)) as MutableList<Int>)
    }
    var pm: MutableMap<Int, Int> = h.pos_map
    (pm)[item] = h.size + 1
    h.size = h.size + 1
    h.arr = arr
    h.pos_map = pm
    heapify_up(h, h.size - 1)
}

fun get_top(h: Heap): MutableList<Int> {
    var arr: MutableList<MutableList<Int>> = h.arr
    if (h.size > 0) {
        return arr[0]!!
    }
    return mutableListOf<Int>()
}

fun extract_top(h: Heap): MutableList<Int> {
    var top: MutableList<Int> = get_top(h)
    if (top.size > 0) {
        delete_item(h, top[0]!!)
    }
    return top
}

fun identity(x: Int): Int {
    return x
}

fun negate(x: Int): Int {
    return 0 - x
}

fun main() {
    insert_item(h, 5, 34)
    insert_item(h, 6, 31)
    insert_item(h, 7, 37)
    println(get_top(h).toString())
    println(extract_top(h).toString())
    println(extract_top(h).toString())
    println(extract_top(h).toString())
    h = new_heap(::negate)
    insert_item(h, 5, 34)
    insert_item(h, 6, 31)
    insert_item(h, 7, 37)
    println(get_top(h).toString())
    println(extract_top(h).toString())
    println(extract_top(h).toString())
    println(extract_top(h).toString())
    insert_item(h, 8, 45)
    insert_item(h, 9, 40)
    insert_item(h, 10, 50)
    println(get_top(h).toString())
    update_item(h, 10, 30)
    println(get_top(h).toString())
    delete_item(h, 10)
    println(get_top(h).toString())
}
