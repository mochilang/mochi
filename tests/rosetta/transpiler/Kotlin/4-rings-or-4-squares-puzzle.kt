val r1: MutableMap<String, Any> = getCombs(1, 7, true)
val r2: MutableMap<String, Any> = getCombs(3, 9, true)
val r3: MutableMap<String, Any> = getCombs(0, 9, false)
fun validComb(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Boolean {
    val square1: Int = a + b
    val square2: Int = (b + c) + d
    val square3: Int = (d + e) + f
    val square4: Int = f + g
    return (((square1 == square2) && (square2 == square3) as Boolean)) && (square3 == square4) as Boolean
}

fun isUnique(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int): Boolean {
    var nums: MutableList<Int> = mutableListOf(a, b, c, d, e, f, g)
    var i: Int = 0
    while (i < nums.size) {
        var j: Int = i + 1
        while (j < nums.size) {
            if (nums[i] == nums[j]) {
                return false as Boolean
            }
            j = j + 1
        }
        i = i + 1
    }
    return true as Boolean
}

fun getCombs(low: Int, high: Int, unique: Boolean): MutableMap<String, Any> {
    var valid: MutableList<Any> = mutableListOf()
    var count: Int = 0
    for (b in low until high + 1) {
        for (c in low until high + 1) {
            for (d in low until high + 1) {
                val s: Int = (b + c) + d
                for (e in low until high + 1) {
                    for (f in low until high + 1) {
                        val a: Int = s - b
                        val g: Int = s - f
                        if ((a < low) || (a > high)) {
                            continue
                        }
                        if ((g < low) || (g > high)) {
                            continue
                        }
                        if (((d + e) + f) != s) {
                            continue
                        }
                        if ((f + g) != s) {
                            continue
                        }
                        if ((!unique as Boolean) || (isUnique(a, b, c, d, e, f, g) as Boolean)) {
                            valid = run { val _tmp = valid.toMutableList(); _tmp.add(mutableListOf(a, b, c, d, e, f, g)); _tmp } as MutableList<Any>
                            count = count + 1
                        }
                    }
                }
            }
        }
    }
    return mutableMapOf<String, Any>("count" to (count), "list" to (valid)) as MutableMap<String, Any>
}

fun main() {
    println((r1)["count"]!!.toString() + " unique solutions in 1 to 7")
    println((r1)["list"]!!)
    println((r2)["count"]!!.toString() + " unique solutions in 3 to 9")
    println((r2)["list"]!!)
    println((r3)["count"]!!.toString() + " non-unique solutions in 0 to 9")
}
