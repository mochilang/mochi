fun absInt(n: Int): Int {
    if (n < 0) {
        return 0 - n
    }
    return n
}

fun user_main(): Unit {
    var b: Int = 1
    while (b <= 5) {
        if (b != 5) {
            var c: Int = 1
            while (c <= 5) {
                if ((c != 1) && (c != b)) {
                    var f: Int = 1
                    while (f <= 5) {
                        if ((((((((f != 1) && (f != 5) as Boolean)) && (f != b) as Boolean)) && (f != c) as Boolean)) && (absInt(f - c) > 1)) {
                            var m: Int = 1
                            while (m <= 5) {
                                if ((((((m != b) && (m != c) as Boolean)) && (m != f) as Boolean)) && (m > c)) {
                                    var s: Int = 1
                                    while (s <= 5) {
                                        if ((((((((s != b) && (s != c) as Boolean)) && (s != f) as Boolean)) && (s != m) as Boolean)) && (absInt(s - f) > 1)) {
                                            println(((((((((("Baker in " + b.toString()) + ", Cooper in ") + c.toString()) + ", Fletcher in ") + f.toString()) + ", Miller in ") + m.toString()) + ", Smith in ") + s.toString()) + ".")
                                            return
                                        }
                                        s = s + 1
                                    }
                                }
                                m = m + 1
                            }
                        }
                        f = f + 1
                    }
                }
                c = c + 1
            }
        }
        b = b + 1
    }
    println("No solution found.")
}

fun main() {
    user_main()
}
