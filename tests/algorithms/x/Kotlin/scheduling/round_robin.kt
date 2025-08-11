import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun calculate_waiting_times(burst_times: MutableList<Int>): MutableList<Int> {
    var quantum: Int = (2).toInt()
    var rem: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < burst_times.size) {
        rem = run { val _tmp = rem.toMutableList(); _tmp.add(burst_times[i]!!); _tmp }
        i = i + 1
    }
    var waiting: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < burst_times.size) {
        waiting = run { val _tmp = waiting.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var t: Int = (0).toInt()
    while (true) {
        var done: Boolean = true
        var j: Int = (0).toInt()
        while (j < burst_times.size) {
            if (rem[j]!! > 0) {
                done = false
                if (rem[j]!! > quantum) {
                    t = t + quantum
                    _listSet(rem, j, rem[j]!! - quantum)
                } else {
                    t = t + rem[j]!!
                    _listSet(waiting, j, t - burst_times[j]!!)
                    _listSet(rem, j, 0)
                }
            }
            j = j + 1
        }
        if ((done as Boolean)) {
            return waiting
        }
    }
    return waiting
}

fun calculate_turn_around_times(burst_times: MutableList<Int>, waiting_times: MutableList<Int>): MutableList<Int> {
    var result: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < burst_times.size) {
        result = run { val _tmp = result.toMutableList(); _tmp.add(burst_times[i]!! + waiting_times[i]!!); _tmp }
        i = i + 1
    }
    return result
}

fun mean(values: MutableList<Int>): Double {
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < values.size) {
        total = total + values[i]!!
        i = i + 1
    }
    return ((total.toDouble())) / ((values.size.toDouble()))
}

fun format_float_5(x: Double): String {
    var scaled: Int = ((((x * 100000.0) + 0.5).toInt())).toInt()
    var int_part: Int = (scaled / 100000).toInt()
    var frac_part: Int = (Math.floorMod(scaled, 100000)).toInt()
    var frac_str: String = frac_part.toString()
    while (frac_str.length < 5) {
        frac_str = "0" + frac_str
    }
    return (int_part.toString() + ".") + frac_str
}

fun user_main(): Unit {
    var burst_times: MutableList<Int> = mutableListOf(3, 5, 7)
    var waiting_times: MutableList<Int> = calculate_waiting_times(burst_times)
    var turn_around_times: MutableList<Int> = calculate_turn_around_times(burst_times, waiting_times)
    println("Process ID \tBurst Time \tWaiting Time \tTurnaround Time")
    var i: Int = (0).toInt()
    while (i < burst_times.size) {
        var line: String = (((((("  " + (i + 1).toString()) + "\t\t  ") + (burst_times[i]!!).toString()) + "\t\t  ") + (waiting_times[i]!!).toString()) + "\t\t  ") + (turn_around_times[i]!!).toString()
        println(line)
        i = i + 1
    }
    println("")
    println("Average waiting time = " + format_float_5(mean(waiting_times)))
    println("Average turn around time = " + format_float_5(mean(turn_around_times)))
}

fun main() {
    user_main()
}
