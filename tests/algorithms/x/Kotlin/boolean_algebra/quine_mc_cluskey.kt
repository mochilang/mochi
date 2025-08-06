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

fun compare_string(string1: String, string2: String): String {
    var result: String = ""
    var count: Int = 0
    var i: Int = 0
    while (i < string1.length) {
        var c1: String = string1.substring(i, i + 1)
        var c2: String = string2.substring(i, i + 1)
        if (c1 != c2) {
            count = count + 1
            result = result + "_"
        } else {
            result = result + c1
        }
        i = i + 1
    }
    if (count > 1) {
        return ""
    }
    return result
}

fun contains_string(arr: MutableList<String>, value: String): Boolean {
    var i: Int = 0
    while (i < arr.size) {
        if (arr[i]!! == value) {
            return true
        }
        i = i + 1
    }
    return false
}

fun unique_strings(arr: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < arr.size) {
        if (!contains_string(res, arr[i]!!)) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(arr[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun check(binary: MutableList<String>): MutableList<String> {
    var pi: MutableList<String> = mutableListOf<String>()
    var current: MutableList<String> = binary
    while (true) {
        var check1: MutableList<String> = mutableListOf<String>()
        var i: Int = 0
        while (i < current.size) {
            check1 = run { val _tmp = check1.toMutableList(); _tmp.add("$"); _tmp }
            i = i + 1
        }
        var temp: MutableList<String> = mutableListOf<String>()
        i = 0
        while (i < current.size) {
            var j: BigInteger = ((i + 1).toBigInteger())
            while (j.compareTo((current.size).toBigInteger()) < 0) {
                var k: String = compare_string(current[i]!!, current[(j).toInt()]!!)
                if (k == "") {
                    _listSet(check1, i, "*")
                    _listSet(check1, (j).toInt(), "*")
                    temp = run { val _tmp = temp.toMutableList(); _tmp.add("X"); _tmp }
                }
                j = j.add((1).toBigInteger())
            }
            i = i + 1
        }
        i = 0
        while (i < current.size) {
            if (check1[i]!! == "$") {
                pi = run { val _tmp = pi.toMutableList(); _tmp.add(current[i]!!); _tmp }
            }
            i = i + 1
        }
        if (temp.size == 0) {
            return pi
        }
        current = unique_strings(temp)
    }
}

fun decimal_to_binary(no_of_variable: Int, minterms: MutableList<Int>): MutableList<String> {
    var temp: MutableList<String> = mutableListOf<String>()
    var idx: Int = 0
    while (idx < minterms.size) {
        var minterm: Int = minterms[idx]!!
        var string: String = ""
        var i: Int = 0
        while (i < no_of_variable) {
            string = (Math.floorMod(minterm, 2)).toString() + string
            minterm = minterm / 2
            i = i + 1
        }
        temp = run { val _tmp = temp.toMutableList(); _tmp.add(string); _tmp }
        idx = idx + 1
    }
    return temp
}

fun is_for_table(string1: String, string2: String, count: Int): Boolean {
    var count_n: Int = 0
    var i: Int = 0
    while (i < string1.length) {
        var c1: String = string1.substring(i, i + 1)
        var c2: String = string2.substring(i, i + 1)
        if (c1 != c2) {
            count_n = count_n + 1
        }
        i = i + 1
    }
    return count_n == count
}

fun count_ones(row: MutableList<Int>): Int {
    var c: Int = 0
    var j: Int = 0
    while (j < row.size) {
        if (row[j]!! == 1) {
            c = c + 1
        }
        j = j + 1
    }
    return c
}

fun selection(chart: MutableList<MutableList<Int>>, prime_implicants: MutableList<String>): MutableList<String> {
    var temp: MutableList<String> = mutableListOf<String>()
    var select: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < chart.size) {
        select = run { val _tmp = select.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var col: Int = 0
    while (col < (chart[0]!!).size) {
        var count: Int = 0
        var row: Int = 0
        while (row < chart.size) {
            if ((((chart[row]!!) as MutableList<Int>))[col]!! == 1) {
                count = count + 1
            }
            row = row + 1
        }
        if (count == 1) {
            var rem: Int = 0
            row = 0
            while (row < chart.size) {
                if ((((chart[row]!!) as MutableList<Int>))[col]!! == 1) {
                    rem = row
                }
                row = row + 1
            }
            _listSet(select, rem, 1)
        }
        col = col + 1
    }
    i = 0
    while (i < select.size) {
        if (select[i]!! == 1) {
            var j: Int = 0
            while (j < (chart[0]!!).size) {
                if ((((chart[i]!!) as MutableList<Int>))[j]!! == 1) {
                    var r: Int = 0
                    while (r < chart.size) {
                        _listSet(chart[r]!!, j, 0)
                        r = r + 1
                    }
                }
                j = j + 1
            }
            temp = run { val _tmp = temp.toMutableList(); _tmp.add(prime_implicants[i]!!); _tmp }
        }
        i = i + 1
    }
    while (true) {
        var counts: MutableList<Int> = mutableListOf<Int>()
        var r: Int = 0
        while (r < chart.size) {
            counts = run { val _tmp = counts.toMutableList(); _tmp.add(count_ones(chart[r]!!)); _tmp }
            r = r + 1
        }
        var max_n: Int = counts[0]!!
        var rem: Int = 0
        var k: Int = 1
        while (k < counts.size) {
            if (counts[k]!! > max_n) {
                max_n = counts[k]!!
                rem = k
            }
            k = k + 1
        }
        if (max_n == 0) {
            return temp
        }
        temp = run { val _tmp = temp.toMutableList(); _tmp.add(prime_implicants[rem]!!); _tmp }
        var j: Int = 0
        while (j < (chart[0]!!).size) {
            if ((((chart[rem]!!) as MutableList<Int>))[j]!! == 1) {
                var r2: Int = 0
                while (r2 < chart.size) {
                    _listSet(chart[r2]!!, j, 0)
                    r2 = r2 + 1
                }
            }
            j = j + 1
        }
    }
}

fun count_char(s: String, ch: String): Int {
    var cnt: Int = 0
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            cnt = cnt + 1
        }
        i = i + 1
    }
    return cnt
}

fun prime_implicant_chart(prime_implicants: MutableList<String>, binary: MutableList<String>): MutableList<MutableList<Int>> {
    var chart: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < prime_implicants.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < binary.size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            j = j + 1
        }
        chart = run { val _tmp = chart.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    i = 0
    while (i < prime_implicants.size) {
        var count: Int = count_char(prime_implicants[i]!!, "_")
        var j: Int = 0
        while (j < binary.size) {
            if (((is_for_table(prime_implicants[i]!!, binary[j]!!, count)) as Boolean)) {
                _listSet(chart[i]!!, j, 1)
            }
            j = j + 1
        }
        i = i + 1
    }
    return chart
}

fun user_main(): Unit {
    var no_of_variable: Int = 3
    var minterms: MutableList<Int> = mutableListOf(1, 5, 7)
    var binary: MutableList<String> = decimal_to_binary(no_of_variable, minterms)
    var prime_implicants: MutableList<String> = check(binary)
    println("Prime Implicants are:")
    println(prime_implicants.toString())
    var chart: MutableList<MutableList<Int>> = prime_implicant_chart(prime_implicants, binary)
    var essential_prime_implicants: MutableList<String> = selection(chart, prime_implicants)
    println("Essential Prime Implicants are:")
    println(essential_prime_implicants.toString())
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
