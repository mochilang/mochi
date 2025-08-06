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

var seed: Int = 1
var NEG_ONE: Int = 0 - 1
fun rand(): Int {
    seed = ((Math.floorMod((((seed * 1103515245) + 12345).toLong()), 2147483648L)).toInt())
    return seed
}

fun randint(a: Int, b: Int): Int {
    var r: Int = rand()
    return a + (Math.floorMod(r, ((b - a) + 1)))
}

fun random(): Double {
    return (1.0 * rand()) / 2147483648.0
}

fun construct_highway(number_of_cells: Int, frequency: Int, initial_speed: Int, random_frequency: Boolean, random_speed: Boolean, max_speed: Int): MutableList<MutableList<Int>> {
    var initial_speed: Int = initial_speed
    var row: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < number_of_cells) {
        row = run { val _tmp = row.toMutableList(); _tmp.add(0 - 1); _tmp }
        i = i + 1
    }
    var highway: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    highway = run { val _tmp = highway.toMutableList(); _tmp.add(row); _tmp }
    i = 0
    if (initial_speed < 0) {
        initial_speed = 0
    }
    while (i < number_of_cells) {
        var speed: Int = initial_speed
        if ((random_speed as Boolean)) {
            speed = randint(0, max_speed)
        }
        _listSet(highway[0]!!, i, speed)
        var step: Int = frequency
        if ((random_frequency as Boolean)) {
            step = randint(1, max_speed * 2)
        }
        i = i + step
    }
    return highway
}

fun get_distance(highway_now: MutableList<Int>, car_index: Int): Int {
    var distance: Int = 0
    var i: BigInteger = ((car_index + 1).toBigInteger())
    while (i.compareTo((highway_now.size).toBigInteger()) < 0) {
        if (highway_now[(i).toInt()]!! > NEG_ONE) {
            return distance
        }
        distance = distance + 1
        i = i.add((1).toBigInteger())
    }
    return distance + get_distance(highway_now, 0 - 1)
}

fun update(highway_now: MutableList<Int>, probability: Double, max_speed: Int): MutableList<Int> {
    var number_of_cells: Int = highway_now.size
    var next_highway: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < number_of_cells) {
        next_highway = run { val _tmp = next_highway.toMutableList(); _tmp.add(0 - 1); _tmp }
        i = i + 1
    }
    var car_index: Int = 0
    while (car_index < number_of_cells) {
        var speed: Int = highway_now[car_index]!!
        if (speed > NEG_ONE) {
            var new_speed: BigInteger = ((speed + 1).toBigInteger())
            if (new_speed.compareTo((max_speed).toBigInteger()) > 0) {
                new_speed = (max_speed.toBigInteger())
            }
            var dn: Int = get_distance(highway_now, car_index) - 1
            if (new_speed.compareTo((dn).toBigInteger()) > 0) {
                new_speed = (dn.toBigInteger())
            }
            if (random() < probability) {
                new_speed = new_speed.subtract((1).toBigInteger())
                if (new_speed.compareTo((0).toBigInteger()) < 0) {
                    new_speed = (0.toBigInteger())
                }
            }
            _listSet(next_highway, car_index, (new_speed.toInt()))
        }
        car_index = car_index + 1
    }
    return next_highway
}

fun simulate(highway: MutableList<MutableList<Int>>, number_of_update: Int, probability: Double, max_speed: Int): MutableList<MutableList<Int>> {
    var highway: MutableList<MutableList<Int>> = highway
    var number_of_cells: Int = (highway[0]!!).size
    var i: Int = 0
    while (i < number_of_update) {
        var next_speeds: MutableList<Int> = update(highway[i]!!, probability, max_speed)
        var real_next: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < number_of_cells) {
            real_next = run { val _tmp = real_next.toMutableList(); _tmp.add(0 - 1); _tmp }
            j = j + 1
        }
        var k: Int = 0
        while (k < number_of_cells) {
            var speed: Int = next_speeds[k]!!
            if (speed > NEG_ONE) {
                var index: Int = Math.floorMod((k + speed), number_of_cells)
                _listSet(real_next, index, speed)
            }
            k = k + 1
        }
        highway = run { val _tmp = highway.toMutableList(); _tmp.add(real_next); _tmp }
        i = i + 1
    }
    return highway
}

fun user_main(): Unit {
    var ex1: MutableList<MutableList<Int>> = simulate(construct_highway(6, 3, 0, false, false, 2), 2, 0.0, 2)
    println(ex1.toString())
    var ex2: MutableList<MutableList<Int>> = simulate(construct_highway(5, 2, 0 - 2, false, false, 2), 3, 0.0, 2)
    println(ex2.toString())
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
