import java.math.BigInteger

val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/data_structures/linked_list"

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

var NIL: Int = (0 - 1).toInt()
var MAX_LEVEL: Int = (6).toInt()
var P: Double = 0.5
var seed: Int = (1).toInt()
var node_keys: MutableList<Int> = mutableListOf<Int>()
var node_vals: MutableList<Int> = mutableListOf<Int>()
var node_forwards: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
var level: Int = (1).toInt()
fun random(): Double {
    seed = (Math.floorMod(((seed * 13) + 7), 100)).toInt()
    return (seed.toDouble()) / 100.0
}

fun random_level(): Int {
    var lvl: Int = (1).toInt()
    while ((random() < P) && (lvl < MAX_LEVEL)) {
        lvl = lvl + 1
    }
    return lvl
}

fun empty_forward(): MutableList<Int> {
    var f: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < MAX_LEVEL) {
        f = run { val _tmp = f.toMutableList(); _tmp.add(NIL.toInt()); _tmp }
        i = i + 1
    }
    return f
}

fun init(): Unit {
    node_keys = mutableListOf(0 - 1)
    node_vals = mutableListOf(0)
    node_forwards = mutableListOf(empty_forward())
    level = (1).toInt()
}

fun insert(key: Int, value: Int): Unit {
    var update: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < MAX_LEVEL) {
        update = run { val _tmp = update.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var x: Int = (0).toInt()
    i = level - 1
    while (i >= 0) {
        while (((((node_forwards[x]!!) as MutableList<Int>)[i]!!).toBigInteger().compareTo((NIL)) != 0) && (node_keys[((node_forwards[x]!!) as MutableList<Int>)[i]!!]!! < key)) {
            x = ((node_forwards[x]!!) as MutableList<Int>)[i]!!
        }
        _listSet(update, i, x)
        i = i - 1
    }
    x = ((node_forwards[x]!!) as MutableList<Int>)[0]!!
    if (((x).toBigInteger().compareTo((NIL)) != 0) && (node_keys[x]!! == key)) {
        _listSet(node_vals, x, value)
        return
    }
    var lvl: Int = (random_level()).toInt()
    if (lvl > level) {
        var j: Int = (level).toInt()
        while (j < lvl) {
            _listSet(update, j, 0)
            j = j + 1
        }
        level = (lvl).toInt()
    }
    node_keys = run { val _tmp = node_keys.toMutableList(); _tmp.add(key); _tmp }
    node_vals = run { val _tmp = node_vals.toMutableList(); _tmp.add(value); _tmp }
    var forwards: MutableList<Int> = empty_forward()
    var idx: Int = (node_keys.size - 1).toInt()
    i = 0
    while (i < lvl) {
        _listSet(forwards, i, ((node_forwards[update[i]!!]!!) as MutableList<Int>)[i]!!)
        _listSet(node_forwards[update[i]!!]!!, i, idx)
        i = i + 1
    }
    node_forwards = run { val _tmp = node_forwards.toMutableList(); _tmp.add(forwards); _tmp }
}

fun find(key: Int): Int {
    var x: Int = (0).toInt()
    var i: Int = (level - 1).toInt()
    while (i >= 0) {
        while (((((node_forwards[x]!!) as MutableList<Int>)[i]!!).toBigInteger().compareTo((NIL)) != 0) && (node_keys[((node_forwards[x]!!) as MutableList<Int>)[i]!!]!! < key)) {
            x = ((node_forwards[x]!!) as MutableList<Int>)[i]!!
        }
        i = i - 1
    }
    x = ((node_forwards[x]!!) as MutableList<Int>)[0]!!
    if (((x).toBigInteger().compareTo((NIL)) != 0) && (node_keys[x]!! == key)) {
        return node_vals[x]!!
    }
    return 0 - 1
}

fun delete(key: Int): Unit {
    var update: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < MAX_LEVEL) {
        update = run { val _tmp = update.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var x: Int = (0).toInt()
    i = level - 1
    while (i >= 0) {
        while (((((node_forwards[x]!!) as MutableList<Int>)[i]!!).toBigInteger().compareTo((NIL)) != 0) && (node_keys[((node_forwards[x]!!) as MutableList<Int>)[i]!!]!! < key)) {
            x = ((node_forwards[x]!!) as MutableList<Int>)[i]!!
        }
        _listSet(update, i, x)
        i = i - 1
    }
    x = ((node_forwards[x]!!) as MutableList<Int>)[0]!!
    if (((x).toBigInteger().compareTo((NIL)) == 0) || (node_keys[x]!! != key)) {
        return
    }
    i = 0
    while (i < level) {
        if (((node_forwards[update[i]!!]!!) as MutableList<Int>)[i]!! == x) {
            _listSet(node_forwards[update[i]!!]!!, i, ((node_forwards[x]!!) as MutableList<Int>)[i]!!)
        }
        i = i + 1
    }
    while ((level > 1) && ((((node_forwards[0]!!) as MutableList<Int>)[level - 1]!!).toBigInteger().compareTo((NIL)) == 0)) {
        level = (level - 1).toInt()
    }
}

fun to_string(): String {
    var s: String = ""
    var x: Int = (((node_forwards[0]!!) as MutableList<Int>)[0]!!).toInt()
    while ((x).toBigInteger().compareTo((NIL)) != 0) {
        if (s != "") {
            s = s + " -> "
        }
        s = ((s + _numToStr(node_keys[x]!!)) + ":") + _numToStr(node_vals[x]!!)
        x = ((node_forwards[x]!!) as MutableList<Int>)[0]!!
    }
    return s
}

fun user_main(): Unit {
    init()
    insert(2, 2)
    insert(4, 4)
    insert(6, 4)
    insert(4, 5)
    insert(8, 4)
    insert(9, 4)
    delete(4)
    println(to_string())
}

fun main() {
    user_main()
}
