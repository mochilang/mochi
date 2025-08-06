import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

var WIDTH: Int = 10
var HEIGHT: Int = 10
var PREY_INITIAL_COUNT: Int = 20
var PREY_REPRODUCTION_TIME: Int = 5
var PREDATOR_INITIAL_COUNT: Int = 5
var PREDATOR_REPRODUCTION_TIME: Int = 20
var PREDATOR_INITIAL_ENERGY: Int = 15
var PREDATOR_FOOD_VALUE: Int = 5
var TYPE_PREY: Int = 0
var TYPE_PREDATOR: Int = 1
var seed: Int = 123456789
var board: MutableList<MutableList<Int>> = create_board()
var entities: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
var dr: MutableList<Int> = mutableListOf(0 - 1, 0, 1, 0)
var dc: MutableList<Int> = mutableListOf(0, 1, 0, 0 - 1)
fun rand(): Int {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())
    return seed
}

fun rand_range(max: Int): Int {
    return Math.floorMod(rand(), max)
}

fun shuffle(list_int: MutableList<Int>): MutableList<Int> {
    var i: BigInteger = ((list_int.size - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) > 0) {
        var j: Int = rand_range(((i.add((1).toBigInteger())).toInt()))
        var tmp: Int = list_int[(i).toInt()]!!
        _listSet(list_int, (i).toInt(), list_int[j]!!)
        _listSet(list_int, j, tmp)
        i = i.subtract((1).toBigInteger())
    }
    return list_int
}

fun create_board(): MutableList<MutableList<Int>> {
    var board: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var r: Int = 0
    while (r < HEIGHT) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var c: Int = 0
        while (c < WIDTH) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            c = c + 1
        }
        board = run { val _tmp = board.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    return board
}

fun create_prey(r: Int, c: Int): MutableList<Int> {
    return mutableListOf(TYPE_PREY, r, c, PREY_REPRODUCTION_TIME, 0, 1)
}

fun create_predator(r: Int, c: Int): MutableList<Int> {
    return mutableListOf(TYPE_PREDATOR, r, c, PREDATOR_REPRODUCTION_TIME, PREDATOR_INITIAL_ENERGY, 1)
}

fun empty_cell(r: Int, c: Int): Boolean {
    return (((board[r]!!) as MutableList<Int>))[c]!! == 0
}

fun add_entity(typ: Int): Unit {
    while (true) {
        var r: Int = rand_range(HEIGHT)
        var c: Int = rand_range(WIDTH)
        if (((empty_cell(r, c)) as Boolean)) {
            if (typ == TYPE_PREY) {
                _listSet(board[r]!!, c, 1)
                entities = run { val _tmp = entities.toMutableList(); _tmp.add(create_prey(r, c)); _tmp }
            } else {
                _listSet(board[r]!!, c, 2)
                entities = run { val _tmp = entities.toMutableList(); _tmp.add(create_predator(r, c)); _tmp }
            }
            return
        }
    }
}

fun setup(): Unit {
    var i: Int = 0
    while (i < PREY_INITIAL_COUNT) {
        add_entity(TYPE_PREY)
        i = i + 1
    }
    i = 0
    while (i < PREDATOR_INITIAL_COUNT) {
        add_entity(TYPE_PREDATOR)
        i = i + 1
    }
}

fun inside(r: Int, c: Int): Boolean {
    return (((((((r >= 0) && (r < HEIGHT) as Boolean)) && (c >= 0) as Boolean)) && (c < WIDTH)) as Boolean)
}

fun find_prey(r: Int, c: Int): Int {
    var i: Int = 0
    while (i < entities.size) {
        var e: MutableList<Int> = entities[i]!!
        if ((((((e[5]!! == 1) && (e[0]!! == TYPE_PREY) as Boolean)) && (e[1]!! == r) as Boolean)) && (e[2]!! == c)) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun step_world(): Unit {
    var i: Int = 0
    while (i < entities.size) {
        var e: MutableList<Int> = entities[i]!!
        if (e[5]!! == 0) {
            i = i + 1
            continue
        }
        var typ: Int = e[0]!!
        var row: Int = e[1]!!
        var col: Int = e[2]!!
        var repro: Int = e[3]!!
        var energy: Int = e[4]!!
        var dirs: MutableList<Int> = mutableListOf(0, 1, 2, 3)
        dirs = shuffle(dirs)
        var moved: Boolean = false
        var old_r: Int = row
        var old_c: Int = col
        if (typ == TYPE_PREDATOR) {
            var j: Int = 0
            var ate: Boolean = false
            while (j < 4) {
                var d: Int = dirs[j]!!
                var nr: Int = row + dr[d]!!
                var nc: Int = col + dc[d]!!
                if (inside(nr, nc) && ((((board[nr]!!) as MutableList<Int>))[nc]!! == 1)) {
                    var prey_index: Int = find_prey(nr, nc)
                    if (prey_index >= 0) {
                        _listSet(entities[prey_index]!!, 5, 0)
                    }
                    _listSet(board[nr]!!, nc, 2)
                    _listSet(board[row]!!, col, 0)
                    _listSet(e, 1, nr)
                    _listSet(e, 2, nc)
                    _listSet(e, 4, (energy + PREDATOR_FOOD_VALUE) - 1)
                    moved = true
                    ate = true
                    break
                }
                j = j + 1
            }
            if (!ate) {
                j = 0
                while (j < 4) {
                    var d: Int = dirs[j]!!
                    var nr: Int = row + dr[d]!!
                    var nc: Int = col + dc[d]!!
                    if (inside(nr, nc) && ((((board[nr]!!) as MutableList<Int>))[nc]!! == 0)) {
                        _listSet(board[nr]!!, nc, 2)
                        _listSet(board[row]!!, col, 0)
                        _listSet(e, 1, nr)
                        _listSet(e, 2, nc)
                        moved = true
                        break
                    }
                    j = j + 1
                }
                _listSet(e, 4, energy - 1)
            }
            if (e[4]!! <= 0) {
                _listSet(e, 5, 0)
                _listSet(board[e[1]!!]!!, e[2]!!, 0)
            }
        } else {
            var j: Int = 0
            while (j < 4) {
                var d: Int = dirs[j]!!
                var nr: Int = row + dr[d]!!
                var nc: Int = col + dc[d]!!
                if (inside(nr, nc) && ((((board[nr]!!) as MutableList<Int>))[nc]!! == 0)) {
                    _listSet(board[nr]!!, nc, 1)
                    _listSet(board[row]!!, col, 0)
                    _listSet(e, 1, nr)
                    _listSet(e, 2, nc)
                    moved = true
                    break
                }
                j = j + 1
            }
        }
        if (e[5]!! == 1) {
            if (moved && (repro <= 0)) {
                if (typ == TYPE_PREY) {
                    _listSet(board[old_r]!!, old_c, 1)
                    entities = run { val _tmp = entities.toMutableList(); _tmp.add(create_prey(old_r, old_c)); _tmp }
                    _listSet(e, 3, PREY_REPRODUCTION_TIME)
                } else {
                    _listSet(board[old_r]!!, old_c, 2)
                    entities = run { val _tmp = entities.toMutableList(); _tmp.add(create_predator(old_r, old_c)); _tmp }
                    _listSet(e, 3, PREDATOR_REPRODUCTION_TIME)
                }
            } else {
                _listSet(e, 3, repro - 1)
            }
        }
        i = i + 1
    }
    var alive: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var k: Int = 0
    while (k < entities.size) {
        var e2: MutableList<Int> = entities[k]!!
        if (e2[5]!! == 1) {
            alive = run { val _tmp = alive.toMutableList(); _tmp.add(e2); _tmp }
        }
        k = k + 1
    }
    entities = alive
}

fun count_entities(typ: Int): Int {
    var cnt: Int = 0
    var i: Int = 0
    while (i < entities.size) {
        if (((((entities[i]!!) as MutableList<Int>))[0]!! == typ) && ((((entities[i]!!) as MutableList<Int>))[5]!! == 1)) {
            cnt = cnt + 1
        }
        i = i + 1
    }
    return cnt
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        setup()
        var t: Int = 0
        while (t < 10) {
            step_world()
            t = t + 1
        }
        println("Prey: " + count_entities(TYPE_PREY).toString())
        println("Predators: " + count_entities(TYPE_PREDATOR).toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
