import java.math.BigInteger

fun bsearch(arr: MutableList<Int>, x: Int): Int {
    var low: Int = 0
    var high: BigInteger = (arr.size - 1).toBigInteger()
    while ((low).toBigInteger().compareTo(high) <= 0) {
        val mid: BigInteger = ((low).toBigInteger().add(high)).divide(2.toBigInteger())
        if (arr[(mid).toInt()] > x) {
            high = mid.subtract(1.toBigInteger())
        } else {
            if (arr[(mid).toInt()] < x) {
                low = (mid.add(1.toBigInteger())).toInt()
            } else {
                return mid.toInt()
            }
        }
    }
    return 0 - 1
}

fun bsearchRec(arr: MutableList<Int>, x: Int, low: Int, high: Int): Int {
    if (high < low) {
        return 0 - 1
    }
    val mid: BigInteger = ((low + high) / 2).toBigInteger()
    if (arr[(mid).toInt()] > x) {
        return bsearchRec(arr, x, low, (mid.subtract(1.toBigInteger())).toInt())
    } else {
        if (arr[(mid).toInt()] < x) {
            return bsearchRec(arr, x, (mid.add(1.toBigInteger())).toInt(), high)
        }
    }
    return mid.toInt()
}

fun user_main(): Unit {
    val nums: MutableList<Int> = mutableListOf(0 - 31, 0, 1, 2, 2, 4, 65, 83, 99, 782)
    var x: Int = 2
    var idx: Int = bsearch(nums, x)
    if (idx >= 0) {
        println(((x.toString() + " is at index ") + idx.toString()) + ".")
    } else {
        println(x.toString() + " is not found.")
    }
    x = 5
    idx = bsearchRec(nums, x, 0, nums.size - 1)
    if (idx >= 0) {
        println(((x.toString() + " is at index ") + idx.toString()) + ".")
    } else {
        println(x.toString() + " is not found.")
    }
}

fun main() {
    user_main()
}
