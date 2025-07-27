import java.math.BigInteger

fun sel(list: MutableList<Double>, k: Int): Double {
    var i: Int = 0
    while (i <= k) {
        var minIndex: Int = i
        var j: BigInteger = (i + 1).toBigInteger()
        while (j.compareTo(list.size.toBigInteger()) < 0) {
            if (list[(j).toInt()] < list[minIndex]) {
                minIndex = j.toInt()
            }
            j = j.add(1.toBigInteger())
        }
        val tmp: Double = list[i]
        list[i] = list[minIndex]
        list[minIndex] = tmp
        i = i + 1
    }
    return list[k]
}

fun median(a: MutableList<Double>): Double {
    var arr: MutableList<Double> = a
    val half: Int = (arr.size / 2).toInt()
    val med: Double = sel(arr, half)
    if ((Math.floorMod(arr.size, 2)) == 0) {
        return (med + arr[half - 1]) / 2.0
    }
    return med
}

fun main() {
    println(median(mutableListOf(3.0, 1.0, 4.0, 1.0)).toString())
    println(median(mutableListOf(3.0, 1.0, 4.0, 1.0, 5.0)).toString())
}
