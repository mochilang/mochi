import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun calculate_waitingtime(arrival_time: MutableList<Int>, burst_time: MutableList<Int>, no_of_processes: Int): MutableList<Int> {
    var remaining_time: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < no_of_processes) {
        remaining_time = run { val _tmp = remaining_time.toMutableList(); _tmp.add(burst_time[i]!!); _tmp }
        i = i + 1
    }
    var waiting_time: MutableList<Int> = mutableListOf<Int>()
    i = 0
    while (i < no_of_processes) {
        waiting_time = run { val _tmp = waiting_time.toMutableList(); _tmp.add(0); _tmp }
        i = i + 1
    }
    var complete: Int = (0).toInt()
    var increment_time: Int = (0).toInt()
    var minm: Int = (1000000000).toInt()
    var short: Int = (0).toInt()
    var check: Boolean = false
    while (complete != no_of_processes) {
        var j: Int = (0).toInt()
        while (j < no_of_processes) {
            if ((((arrival_time[j]!! <= increment_time) && (remaining_time[j]!! > 0) as Boolean)) && (remaining_time[j]!! < minm)) {
                minm = remaining_time[j]!!
                short = j
                check = true
            }
            j = j + 1
        }
        if (!check) {
            increment_time = increment_time + 1
            continue
        }
        _listSet(remaining_time, short, remaining_time[short]!! - 1)
        minm = remaining_time[short]!!
        if (minm == 0) {
            minm = 1000000000
        }
        if (remaining_time[short]!! == 0) {
            complete = complete + 1
            check = false
            var finish_time: Int = (increment_time + 1).toInt()
            var finar: Int = (finish_time - arrival_time[short]!!).toInt()
            _listSet(waiting_time, short, finar - burst_time[short]!!)
            if (waiting_time[short]!! < 0) {
                _listSet(waiting_time, short, 0)
            }
        }
        increment_time = increment_time + 1
    }
    return waiting_time
}

fun calculate_turnaroundtime(burst_time: MutableList<Int>, no_of_processes: Int, waiting_time: MutableList<Int>): MutableList<Int> {
    var turn_around_time: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < no_of_processes) {
        turn_around_time = run { val _tmp = turn_around_time.toMutableList(); _tmp.add(burst_time[i]!! + waiting_time[i]!!); _tmp }
        i = i + 1
    }
    return turn_around_time
}

fun to_float(x: Int): Double {
    return x * 1.0
}

fun calculate_average_times(waiting_time: MutableList<Int>, turn_around_time: MutableList<Int>, no_of_processes: Int): Unit {
    var total_waiting_time: Int = (0).toInt()
    var total_turn_around_time: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < no_of_processes) {
        total_waiting_time = total_waiting_time + waiting_time[i]!!
        total_turn_around_time = total_turn_around_time + turn_around_time[i]!!
        i = i + 1
    }
    var avg_wait: Double = to_float(total_waiting_time) / to_float(no_of_processes)
    var avg_turn: Double = to_float(total_turn_around_time) / to_float(no_of_processes)
    println("Average waiting time = " + avg_wait.toString())
    println("Average turn around time = " + avg_turn.toString())
}

fun main() {
    println(calculate_waitingtime(mutableListOf(1, 2, 3, 4), mutableListOf(3, 3, 5, 1), 4))
    println(calculate_waitingtime(mutableListOf(1, 2, 3), mutableListOf(2, 5, 1), 3))
    println(calculate_waitingtime(mutableListOf(2, 3), mutableListOf(5, 1), 2))
    println(calculate_turnaroundtime(mutableListOf(3, 3, 5, 1), 4, mutableListOf(0, 3, 5, 0)))
    println(calculate_turnaroundtime(mutableListOf(3, 3), 2, mutableListOf(0, 3)))
    println(calculate_turnaroundtime(mutableListOf(8, 10, 1), 3, mutableListOf(1, 0, 3)))
    calculate_average_times(mutableListOf(0, 3, 5, 0), mutableListOf(3, 6, 10, 1), 4)
    calculate_average_times(mutableListOf(2, 3), mutableListOf(3, 6), 2)
    calculate_average_times(mutableListOf(10, 4, 3), mutableListOf(2, 7, 6), 3)
}
