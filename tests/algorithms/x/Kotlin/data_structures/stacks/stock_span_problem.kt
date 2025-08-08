import java.math.BigInteger

var price: MutableList<Int> = mutableListOf(10, 4, 5, 90, 120, 80)
var spans: MutableList<Int> = calculation_span(price)
fun calculation_span(price: MutableList<Int>): MutableList<Int> {
    var n: Int = price.size
    var st: MutableList<Int> = mutableListOf<Int>()
    var span: MutableList<Int> = mutableListOf<Int>()
    st = run { val _tmp = st.toMutableList(); _tmp.add(0); _tmp }
    span = run { val _tmp = span.toMutableList(); _tmp.add(1); _tmp }
    for (i in 1 until n) {
        while ((st.size > 0) && (price[st[st.size - 1]!!]!! <= price[i]!!)) {
            st = st.subList(0, st.size - 1)
        }
        var s = if (st.size <= 0) i + 1 else i - st[st.size - 1]!!
        span = run { val _tmp = span.toMutableList(); _tmp.add((s.toInt())); _tmp }
        st = run { val _tmp = st.toMutableList(); _tmp.add(i); _tmp }
    }
    return span
}

fun print_array(arr: MutableList<Int>): Unit {
    for (i in 0 until arr.size) {
        println(arr[i]!!)
    }
}

fun main() {
    print_array(spans)
}
