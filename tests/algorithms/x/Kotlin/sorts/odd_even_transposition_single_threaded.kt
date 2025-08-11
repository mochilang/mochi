import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun odd_even_transposition(arr: MutableList<Double>): MutableList<Double> {
    var n: Int = (arr.size).toInt()
    var pass: Int = (0).toInt()
    while (pass < n) {
        var i: BigInteger = ((Math.floorMod(pass, 2)).toBigInteger())
        while (i.compareTo((n - 1).toBigInteger()) < 0) {
            if (arr[(i.add((1).toBigInteger())).toInt()]!! < arr[(i).toInt()]!!) {
                var tmp: Double = arr[(i).toInt()]!!
                _listSet(arr, (i).toInt(), arr[(i.add((1).toBigInteger())).toInt()]!!)
                _listSet(arr, (i.add((1).toBigInteger())).toInt(), tmp)
            }
            i = i.add((2).toBigInteger())
        }
        pass = pass + 1
    }
    return arr
}

fun main() {
    println(odd_even_transposition(mutableListOf(5.0, 4.0, 3.0, 2.0, 1.0)).toString())
    println(odd_even_transposition(mutableListOf(13.0, 11.0, 18.0, 0.0, 0.0 - 1.0)).toString())
    println(odd_even_transposition(mutableListOf(0.0 - 0.1, 1.1, 0.1, 0.0 - 2.9)).toString())
}
