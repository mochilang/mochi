import java.math.BigInteger

fun min3(a: Int, b: Int, c: Int): Int {
    var m: Int = (a).toInt()
    if (b < m) {
        m = b
    }
    if (c < m) {
        m = c
    }
    return m
}

fun edit_distance(source: String, target: String): Int {
    if (source.length == 0) {
        return target.length
    }
    if (target.length == 0) {
        return source.length
    }
    var last_source: String = source.substring(source.length - 1, source.length)
    var last_target: String = target.substring(target.length - 1, target.length)
    var delta: Int = (if (last_source == last_target) 0 else 1).toInt()
    var delete_cost: Int = (edit_distance(source.substring(0, source.length - 1), target) + 1).toInt()
    var insert_cost: Int = (edit_distance(source, target.substring(0, target.length - 1)) + 1).toInt()
    var replace_cost: Int = (edit_distance(source.substring(0, source.length - 1), target.substring(0, target.length - 1)) + delta).toInt()
    return min3(delete_cost, insert_cost, replace_cost)
}

fun user_main(): Unit {
    var result: Int = (edit_distance("ATCGCTG", "TAGCTAA")).toInt()
    println(result.toString())
}

fun main() {
    user_main()
}
