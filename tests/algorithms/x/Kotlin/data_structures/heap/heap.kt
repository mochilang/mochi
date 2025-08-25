import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/heap"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

var heap: MutableList<Double> = mutableListOf(103.0, 9.0, 1.0, 7.0, 11.0, 15.0, 25.0, 201.0, 209.0, 107.0, 5.0)
var size: Int = (build_max_heap(heap)).toInt()
fun parent_index(child_idx: Int): Int {
    if (child_idx > 0) {
        return Math.floorDiv((child_idx - 1), 2)
    }
    return 0 - 1
}

fun left_child_idx(parent_idx: Int): Int {
    return (2 * parent_idx) + 1
}

fun right_child_idx(parent_idx: Int): Int {
    return (2 * parent_idx) + 2
}

fun max_heapify(h: MutableList<Double>, heap_size: Int, index: Int): Unit {
    var largest: Int = (index).toInt()
    var left: Int = (left_child_idx(index)).toInt()
    var right: Int = (right_child_idx(index)).toInt()
    if ((left < heap_size) && (h[left]!! > h[largest]!!)) {
        largest = left
    }
    if ((right < heap_size) && (h[right]!! > h[largest]!!)) {
        largest = right
    }
    if (largest != index) {
        var temp: Double = h[index]!!
        _listSet(h, index, h[largest]!!)
        _listSet(h, largest, temp)
        max_heapify(h, heap_size, largest)
    }
}

fun build_max_heap(h: MutableList<Double>): Int {
    var heap_size: Int = (h.size).toInt()
    var i: Int = ((Math.floorDiv(heap_size, 2)) - 1).toInt()
    while (i >= 0) {
        max_heapify(h, heap_size, i)
        i = i - 1
    }
    return heap_size
}

fun extract_max(h: MutableList<Double>, heap_size: Int): Double {
    var max_value: Double = h[0]!!
    _listSet(h, 0, h[heap_size - 1]!!)
    max_heapify(h, heap_size - 1, 0)
    return max_value
}

fun insert(h: MutableList<Double>, heap_size: Int, value: Double): Int {
    var heap_size: Int = (heap_size).toInt()
    var h: MutableList<Double> = h
    if (heap_size < h.size) {
        _listSet(h, heap_size, value)
    } else {
        h = run { val _tmp = h.toMutableList(); _tmp.add(value); _tmp }
    }
    heap_size = heap_size + 1
    var idx: Int = (Math.floorDiv((heap_size - 1), 2)).toInt()
    while (idx >= 0) {
        max_heapify(h, heap_size, idx)
        idx = Math.floorDiv((idx - 1), 2)
    }
    return heap_size
}

fun heap_sort(h: MutableList<Double>, heap_size: Int): Unit {
    var size: Int = (heap_size).toInt()
    var j: Int = (size - 1).toInt()
    while (j > 0) {
        var temp: Double = h[0]!!
        _listSet(h, 0, h[j]!!)
        _listSet(h, j, temp)
        size = (size - 1).toInt()
        max_heapify(h, size, 0)
        j = j - 1
    }
}

fun heap_to_string(h: MutableList<Double>, heap_size: Int): String {
    var s: String = "["
    var i: Int = (0).toInt()
    while (i < heap_size) {
        s = s + _numToStr(h[i]!!)
        if (i < (heap_size - 1)) {
            s = s + ", "
        }
        i = i + 1
    }
    s = s + "]"
    return s
}

fun main() {
    println(heap_to_string(heap, size))
    var m: Double = extract_max(heap, size)
    size = (size - 1).toInt()
    println(_numToStr(m))
    println(heap_to_string(heap, size))
    size = (insert(heap, size, 100.0)).toInt()
    println(heap_to_string(heap, size))
    heap_sort(heap, size)
    println(heap_to_string(heap, size))
}
