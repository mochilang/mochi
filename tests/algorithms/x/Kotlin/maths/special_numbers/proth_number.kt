import java.math.BigInteger

fun pow2(n: Int): Int {
var v = 1
var i = 0
while (i < n) {
v *= 2
i++
}
return v
}

fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun proth(number: Int): Int {
    if (number < 1) {
        panic("Input value must be > 0")
    }
    if (number == 1) {
        return 3
    }
    if (number == 2) {
        return 5
    }
    var temp: Int = ((number / 3).toInt()).toInt()
    var pow: Int = (1).toInt()
    var block_index: Int = (1).toInt()
    while (pow <= temp) {
        pow = pow * 2
        block_index = block_index + 1
    }
    var proth_list: MutableList<Int> = mutableListOf(3, 5)
    var proth_index: Int = (2).toInt()
    var increment: Int = (3).toInt()
    var block: Int = (1).toInt()
    while (block < block_index) {
        var i: Int = (0).toInt()
        while (i < increment) {
            var next_val: Int = (pow2(block + 1) + proth_list[proth_index - 1]!!).toInt()
            proth_list = run { val _tmp = proth_list.toMutableList(); _tmp.add(next_val); _tmp }
            proth_index = proth_index + 1
            i = i + 1
        }
        increment = increment * 2
        block = block + 1
    }
    return proth_list[number - 1]!!
}

fun user_main(): Unit {
    var n: Int = (1).toInt()
    while (n <= 10) {
        var value: Int = (proth(n)).toInt()
        println((("The " + _numToStr(n)) + "th Proth number: ") + _numToStr(value))
        n = n + 1
    }
}

fun main() {
    user_main()
}
