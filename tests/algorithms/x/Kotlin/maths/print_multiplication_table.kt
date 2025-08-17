fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun multiplication_table(number: Int, number_of_terms: Int): String {
    var i: Int = (1).toInt()
    var result: String = ""
    while (i <= number_of_terms) {
        result = ((((result + _numToStr(number)) + " * ") + _numToStr(i)) + " = ") + _numToStr(number * i)
        if (i < number_of_terms) {
            result = result + "\n"
        }
        i = i + 1
    }
    return result
}

fun main() {
    println(multiplication_table(5, 10))
}
