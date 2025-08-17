fun panic(msg: String): Nothing { throw RuntimeException(msg) }

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

var PI: Double = 3.141592653589793
fun radians(degree: Double): Double {
    return degree / (180.0 / PI)
}

fun abs_float(x: Double): Double {
    if (x < 0.0) {
        return 0.0 - x
    }
    return x
}

fun almost_equal(a: Double, b: Double): Boolean {
    return abs_float(a - b) <= 0.00000001
}

fun test_radians(): Unit {
    if (!almost_equal(radians(180.0), PI)) {
        panic("radians 180 failed")
    }
    if (!almost_equal(radians(92.0), 1.6057029118347832)) {
        panic("radians 92 failed")
    }
    if (!almost_equal(radians(274.0), 4.782202150464463)) {
        panic("radians 274 failed")
    }
    if (!almost_equal(radians(109.82), 1.9167205845401725)) {
        panic("radians 109.82 failed")
    }
}

fun user_main(): Unit {
    test_radians()
    println(_numToStr(radians(180.0)))
    println(_numToStr(radians(92.0)))
    println(_numToStr(radians(274.0)))
    println(_numToStr(radians(109.82)))
}

fun main() {
    user_main()
}
