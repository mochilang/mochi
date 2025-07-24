import java.math.BigInteger

fun poolPut(p: MutableList<Int>, x: Int): MutableList<Int> {
    return run { val _tmp = p.toMutableList(); _tmp.add(x); _tmp } as MutableList<Int>
}

fun poolGet(p: MutableList<Int>): MutableMap<String, Any?> {
    var p: MutableList<Int> = p
    if (p.size == 0) {
        println("pool empty")
        return mutableMapOf<String, Any?>("pool" to (p), "val" to (0))
    }
    val idx: BigInteger = p.size - 1
    val v: Int = (p)[idx] as Int
    p = p.subList(0, idx)
    return mutableMapOf<String, Any?>("pool" to (p), "val" to (v))
}

fun clearPool(p: MutableList<Int>): MutableList<Int> {
    return mutableListOf() as MutableList<Int>
}

fun user_main(): Unit {
    var pool: MutableList<Int> = mutableListOf()
    var i: Int = 1
    var j: Int = 2
    println((i + j).toString())
    pool = poolPut(pool, i)
    pool = poolPut(pool, j)
    i = 0
    j = 0
    val res1: MutableMap<String, Any?> = poolGet(pool)
    pool = (res1)["pool"] as Any? as MutableList<Int>
    i = (res1)["val"] as Any?.toInt()
    val res2: MutableMap<String, Any?> = poolGet(pool)
    pool = (res2)["pool"] as Any? as MutableList<Int>
    j = (res2)["val"] as Any?.toInt()
    i = 4
    j = 5
    println((i + j).toString())
    pool = poolPut(pool, i)
    pool = poolPut(pool, j)
    i = 0
    j = 0
    pool = clearPool(pool)
    val res3: MutableMap<String, Any?> = poolGet(pool)
    pool = (res3)["pool"] as Any? as MutableList<Int>
    i = (res3)["val"] as Any?.toInt()
    val res4: MutableMap<String, Any?> = poolGet(pool)
    pool = (res4)["pool"] as Any? as MutableList<Int>
    j = (res4)["val"] as Any?.toInt()
    i = 7
    j = 8
    println((i + j).toString())
}

fun main() {
    user_main()
}
