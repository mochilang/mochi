var arr1: MutableList<Int> = mutableListOf(2, 7, 1, 8, 2)
var counts1: MutableMap<Int, Int> = mutableMapOf<Any?, Any?>() as MutableMap<Int, Int>
var keys1: MutableList<Int> = mutableListOf()
var i: Int = 0
var max1: Int = 0
var modes1: MutableList<Int> = mutableListOf()
var arr2: MutableList<Int> = mutableListOf(2, 7, 1, 8, 2, 8)
var counts2: MutableMap<Int, Int> = mutableMapOf<Any?, Any?>() as MutableMap<Int, Int>
var keys2: MutableList<Int> = mutableListOf()
var max2: Int = 0
var modes2: MutableList<Int> = mutableListOf()
fun main() {
    while (i < arr1.size) {
        val v: Int = arr1[i]
        if (v in counts1) {
            (counts1)[v] = (counts1)[v] as Int + 1
        } else {
            (counts1)[v] = 1
            keys1 = run { val _tmp = keys1.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
        }
        i = i + 1
    }
    i = 0
    while (i < keys1.size) {
        val k: Int = keys1[i]
        val c: Int = (counts1)[k] as Int
        if (c > max1) {
            max1 = c
        }
        i = i + 1
    }
    i = 0
    while (i < keys1.size) {
        val k: Int = keys1[i]
        if ((counts1)[k] as Int == max1) {
            modes1 = run { val _tmp = modes1.toMutableList(); _tmp.add(k); _tmp } as MutableList<Int>
        }
        i = i + 1
    }
    println(modes1.toString())
    i = 0
    while (i < arr2.size) {
        val v: Int = arr2[i]
        if (v in counts2) {
            (counts2)[v] = (counts2)[v] as Int + 1
        } else {
            (counts2)[v] = 1
            keys2 = run { val _tmp = keys2.toMutableList(); _tmp.add(v); _tmp } as MutableList<Int>
        }
        i = i + 1
    }
    i = 0
    while (i < keys2.size) {
        val k: Int = keys2[i]
        val c: Int = (counts2)[k] as Int
        if (c > max2) {
            max2 = c
        }
        i = i + 1
    }
    i = 0
    while (i < keys2.size) {
        val k: Int = keys2[i]
        if ((counts2)[k] as Int == max2) {
            modes2 = run { val _tmp = modes2.toMutableList(); _tmp.add(k); _tmp } as MutableList<Int>
        }
        i = i + 1
    }
    println(modes2.toString())
}
