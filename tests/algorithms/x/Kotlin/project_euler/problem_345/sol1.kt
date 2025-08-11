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

var MATRIX_2: MutableList<String> = mutableListOf("7 53 183 439 863 497 383 563 79 973 287 63 343 169 583", "627 343 773 959 943 767 473 103 699 303 957 703 583 639 913", "447 283 463 29 23 487 463 993 119 883 327 493 423 159 743", "217 623 3 399 853 407 103 983 89 463 290 516 212 462 350", "960 376 682 962 300 780 486 502 912 800 250 346 172 812 350", "870 456 192 162 593 473 915 45 989 873 823 965 425 329 803", "973 965 905 919 133 673 665 235 509 613 673 815 165 992 326", "322 148 972 962 286 255 941 541 265 323 925 281 601 95 973", "445 721 11 525 473 65 511 164 138 672 18 428 154 448 848", "414 456 310 312 798 104 566 520 302 248 694 976 430 392 198", "184 829 373 181 631 101 969 613 840 740 778 458 284 760 390", "821 461 843 513 17 901 711 993 293 157 274 94 192 156 574", "34 124 4 878 450 476 712 914 838 669 875 299 823 329 699", "815 559 813 459 522 788 168 586 966 232 308 833 251 631 107", "813 883 451 509 615 77 281 613 459 205 380 274 302 35 805")
var result: Int = (solution(MATRIX_2)).toInt()
fun parse_row(row_str: String): MutableList<Int> {
    var nums: MutableList<Int> = mutableListOf<Int>()
    var current: Int = (0).toInt()
    var has_digit: Boolean = false
    var i: Int = (0).toInt()
    while (i < row_str.length) {
        var ch: String = row_str.substring(i, i + 1)
        if (ch == " ") {
            if ((has_digit as Boolean)) {
                nums = run { val _tmp = nums.toMutableList(); _tmp.add(current); _tmp }
                current = 0
                has_digit = false
            }
        } else {
            current = (current * 10) + ((ch.toBigInteger().toInt()))
            has_digit = true
        }
        i = i + 1
    }
    if ((has_digit as Boolean)) {
        nums = run { val _tmp = nums.toMutableList(); _tmp.add(current); _tmp }
    }
    return nums
}

fun parse_matrix(matrix_str: MutableList<String>): MutableList<MutableList<Int>> {
    var matrix: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    for (row_str in matrix_str) {
        var row: MutableList<Int> = parse_row(row_str)
        matrix = run { val _tmp = matrix.toMutableList(); _tmp.add(row); _tmp }
    }
    return matrix
}

fun bitcount(x: Int): Int {
    var count: Int = (0).toInt()
    var y: Int = (x).toInt()
    while (y > 0) {
        if ((Math.floorMod(y, 2)) == 1) {
            count = count + 1
        }
        y = y / 2
    }
    return count
}

fun build_powers(n: Int): MutableList<Int> {
    var powers: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    var current: Int = (1).toInt()
    while (i <= n) {
        powers = run { val _tmp = powers.toMutableList(); _tmp.add(current); _tmp }
        current = current * 2
        i = i + 1
    }
    return powers
}

fun solution(matrix_str: MutableList<String>): Int {
    var arr: MutableList<MutableList<Int>> = parse_matrix(matrix_str)
    var n: Int = (arr.size).toInt()
    var powers: MutableList<Int> = build_powers(n)
    var size: Int = (powers[n]!!).toInt()
    var dp: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < size) {
        dp = run { val _tmp = dp.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var mask: Int = (0).toInt()
    while (mask < size) {
        var row: Int = (bitcount(mask)).toInt()
        if (row < n) {
            var col: Int = (0).toInt()
            while (col < n) {
                if ((Math.floorMod((mask / powers[col]!!), 2)) == 0) {
                    var new_mask: Int = (mask + powers[col]!!).toInt()
                    var value: Int = (dp[mask]!! + (((arr[row]!!) as MutableList<Int>))[col]!!).toInt()
                    if (value > dp[new_mask]!!) {
                        _listSet(dp, new_mask, value)
                    }
                }
                col = col + 1
            }
        }
        mask = mask + 1
    }
    return dp[size - 1]!!
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println("solution() = " + result.toString())
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
