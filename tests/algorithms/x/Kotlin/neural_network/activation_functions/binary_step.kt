fun binary_step(vector: MutableList<Double>): MutableList<Int> {
    var out: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i < vector.size) {
        if (vector[i]!! >= 0.0) {
            out = run { val _tmp = out.toMutableList(); _tmp.add(1); _tmp }
        } else {
            out = run { val _tmp = out.toMutableList(); _tmp.add(0); _tmp }
        }
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var vector: MutableList<Double> = mutableListOf(0.0 - 1.2, 0.0, 2.0, 1.45, 0.0 - 3.7, 0.3)
    var result: MutableList<Int> = binary_step(vector)
    println(result)
}

fun main() {
    user_main()
}
