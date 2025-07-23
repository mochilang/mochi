var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Int {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        _nowSeed.toInt()
    } else {
        System.nanoTime().toInt()
    }
}

val OP_NUM: Int = 0
val OP_ADD: Int = 1
val OP_SUB: Int = 2
val OP_MUL: Int = 3
val OP_DIV: Int = 4
val n_cards: Int = 4
val goal: Int = 24
val digit_range: Int = 9
fun newNum(n: Int): MutableMap<String, Any> {
    return mutableMapOf<String, Any>("op" to (OP_NUM), "value" to (mutableMapOf<String, Int>("num" to (n), "denom" to (1)))) as MutableMap<String, Any>
}

fun exprEval(x: MutableMap<String, Any>): MutableMap<String, Int> {
    if ((x)["op"]!! == OP_NUM) {
        return (x)["value"]!! as MutableMap<String, Int>
    }
    val l: MutableMap<String, Int> = exprEval((x)["left"]!! as MutableMap<String, Any>)
    val r: MutableMap<String, Int> = exprEval((x)["right"]!! as MutableMap<String, Any>)
    if ((x)["op"]!! == OP_ADD) {
        return mutableMapOf<String, Int>("num" to (((l)["num"] as Int * (r)["denom"] as Int) + ((l)["denom"] as Int * (r)["num"] as Int)), "denom" to ((l)["denom"] as Int * (r)["denom"] as Int)) as MutableMap<String, Int>
    }
    if ((x)["op"]!! == OP_SUB) {
        return mutableMapOf<String, Int>("num" to (((l)["num"] as Int * (r)["denom"] as Int) - ((l)["denom"] as Int * (r)["num"] as Int)), "denom" to ((l)["denom"] as Int * (r)["denom"] as Int)) as MutableMap<String, Int>
    }
    if ((x)["op"]!! == OP_MUL) {
        return mutableMapOf<String, Int>("num" to ((l)["num"] as Int * (r)["num"] as Int), "denom" to ((l)["denom"] as Int * (r)["denom"] as Int)) as MutableMap<String, Int>
    }
    return mutableMapOf<String, Int>("num" to ((l)["num"] as Int * (r)["denom"] as Int), "denom" to ((l)["denom"] as Int * (r)["num"] as Int)) as MutableMap<String, Int>
}

fun exprString(x: MutableMap<String, Any>): String {
    if ((x)["op"]!! == OP_NUM) {
        return ((x)["value"] as MutableMap<String, Any>)["num"]!!.toString() as String
    }
    val ls: String = exprString((x)["left"]!! as MutableMap<String, Any>)
    val rs: String = exprString((x)["right"]!! as MutableMap<String, Any>)
    var opstr: String = ""
    if ((x)["op"]!! == OP_ADD) {
        opstr = " + "
    } else {
        if ((x)["op"]!! == OP_SUB) {
            opstr = " - "
        } else {
            if ((x)["op"]!! == OP_MUL) {
                opstr = " * "
            } else {
                opstr = " / "
            }
        }
    }
    return ((("(" + ls) + opstr) + rs) + ")" as String
}

fun solve(xs: MutableList<MutableMap<String, Any>>): Boolean {
    if (xs.size == 1) {
        val f: MutableMap<String, Int> = exprEval(xs[0])
        if (((f)["denom"] as Int != 0) && ((f)["num"] as Int == ((f)["denom"] as Int * goal))) {
            println(exprString(xs[0]))
            return true as Boolean
        }
        return false as Boolean
    }
    var i: Int = 0
    while (i < xs.size) {
        var j: Int = i + 1
        while (j < xs.size) {
            var rest: MutableList<MutableMap<String, Any>> = mutableListOf()
            var k: Int = 0
            while (k < xs.size) {
                if ((k != i) && (k != j)) {
                    rest = run { val _tmp = rest.toMutableList(); _tmp.add(xs[k]); _tmp } as MutableList<MutableMap<String, Any>>
                }
                k = k + 1
            }
            val a: MutableMap<String, Any> = xs[i]
            val b: MutableMap<String, Any> = xs[j]
            for (op in mutableListOf(OP_ADD, OP_SUB, OP_MUL, OP_DIV)) {
                var node: MutableMap<String, Any> = mutableMapOf<String, Any>("op" to (op), "left" to (a), "right" to (b))
                if (solve(run { val _tmp = rest.toMutableList(); _tmp.add(node); _tmp } as MutableList<MutableMap<String, Any>>) as Boolean) {
                    return true as Boolean
                }
            }
            var node: MutableMap<String, Any> = mutableMapOf<String, Any>("op" to (OP_SUB), "left" to (b), "right" to (a))
            if (solve(run { val _tmp = rest.toMutableList(); _tmp.add(node); _tmp } as MutableList<MutableMap<String, Any>>) as Boolean) {
                return true as Boolean
            }
            node = mutableMapOf<String, Any>("op" to (OP_DIV), "left" to (b), "right" to (a))
            if (solve(run { val _tmp = rest.toMutableList(); _tmp.add(node); _tmp } as MutableList<MutableMap<String, Any>>) as Boolean) {
                return true as Boolean
            }
            j = j + 1
        }
        i = i + 1
    }
    return false as Boolean
}

fun user_main(): Unit {
    var iter: Int = 0
    while (iter < 10) {
        var cards: MutableList<MutableMap<String, Any>> = mutableListOf()
        var i: Int = 0
        while (i < n_cards) {
            val n: Int = (_now() % (digit_range - 1)) + 1
            cards = run { val _tmp = cards.toMutableList(); _tmp.add(newNum(n)); _tmp } as MutableList<MutableMap<String, Any>>
            println(" " + n.toString())
            i = i + 1
        }
        println(":  ")
        if (!(solve(cards) as Boolean)) {
            println("No solution")
        }
        iter = iter + 1
    }
}

fun main() {
    user_main()
}
