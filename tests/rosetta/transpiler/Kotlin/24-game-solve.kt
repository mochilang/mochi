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
        kotlin.math.abs(_nowSeed.toInt())
    } else {
        kotlin.math.abs(System.nanoTime().toInt())
    }
}

fun toJson(v: Any?): String = when (v) {
    null -> "null"
    is String -> "\"" + v.replace("\"", "\\\"") + "\""
    is Boolean, is Number -> v.toString()
    is Map<*, *> -> v.entries.joinToString(prefix = "{", postfix = "}") { toJson(it.key.toString()) + ":" + toJson(it.value) }
    is Iterable<*> -> v.joinToString(prefix = "[", postfix = "]") { toJson(it) }
    else -> toJson(v.toString())
}

sealed class Expr
data class Num(var value: Rational) : Expr()
data class Bin(var op: Int, var left: Expr, var right: Expr) : Expr()
data class Rational(var num: Int, var denom: Int)
val OP_ADD: Int = 1
val OP_SUB: Int = 2
val OP_MUL: Int = 3
val OP_DIV: Int = 4
val n_cards: Int = 4
val goal: Int = 24
val digit_range: Int = 9
fun binEval(op: Int, l: Expr, r: Expr): Rational {
    val lv: Rational = exprEval(l)
    val rv: Rational = exprEval(r)
    if (op == OP_ADD) {
        return Rational(num = (lv.num * rv.denom) + (lv.denom * rv.num), denom = lv.denom * rv.denom)
    }
    if (op == OP_SUB) {
        return Rational(num = (lv.num * rv.denom) - (lv.denom * rv.num), denom = lv.denom * rv.denom)
    }
    if (op == OP_MUL) {
        return Rational(num = lv.num * rv.num, denom = lv.denom * rv.denom)
    }
    return Rational(num = lv.num * rv.denom, denom = lv.denom * rv.num)
}

fun binString(op: Int, l: Expr, r: Expr): String {
    val ls: String = exprString(l)
    val rs: String = exprString(r)
    var opstr: String = ""
    if (op == OP_ADD) {
        opstr = " + "
    } else {
        if (op == OP_SUB) {
            opstr = " - "
        } else {
            if (op == OP_MUL) {
                opstr = " * "
            } else {
                opstr = " / "
            }
        }
    }
    return ((("(" + ls) + opstr) + rs) + ")"
}

fun newNum(n: Int): Expr {
    return Num(value = Rational(num = n, denom = 1)) as Expr
}

fun exprEval(x: Expr): Rational {
    return when (x) {
    is Num -> run {
    val v: Rational = (x as Num).value
    v
}
    is Bin -> run {
    val op: Int = (x as Bin).op
    val l: Expr = (x as Bin).left
    val r: Expr = (x as Bin).right
    binEval(op, l, r)
}
} as Rational
}

fun exprString(x: Expr): String {
    return when (x) {
    is Num -> run {
    val v: Rational = (x as Num).value
    v.num.toString()
}
    is Bin -> run {
    val op: Int = (x as Bin).op
    val l: Expr = (x as Bin).left
    val r: Expr = (x as Bin).right
    binString(op, l, r)
}
} as String
}

fun solve(xs: MutableList<Expr>): Boolean {
    if (xs.size == 1) {
        val f: Rational = exprEval(xs[0])
        if ((f.denom != 0) && (f.num == (f.denom * goal))) {
            println(exprString(xs[0]))
            return true
        }
        return false
    }
    var i: Int = 0
    while (i < xs.size) {
        var j: Int = i + 1
        while (j < xs.size) {
            var rest: MutableList<Expr> = mutableListOf()
            var k: Int = 0
            while (k < xs.size) {
                if ((k != i) && (k != j)) {
                    rest = run { val _tmp = rest.toMutableList(); _tmp.add(xs[k]); _tmp } as MutableList<Expr>
                }
                k = k + 1
            }
            val a: Expr = xs[i]
            val b: Expr = xs[j]
            var node: Bin = Bin(op = OP_ADD, left = a, right = b)
            for (op in mutableListOf(OP_ADD, OP_SUB, OP_MUL, OP_DIV)) {
                node = Bin(op = op, left = a, right = b)
                if ((solve(run { val _tmp = rest.toMutableList(); _tmp.add(node); _tmp } as MutableList<Expr>)) as Boolean) {
                    return true
                }
            }
            node = Bin(op = OP_SUB, left = b, right = a)
            if ((solve(run { val _tmp = rest.toMutableList(); _tmp.add(node); _tmp } as MutableList<Expr>)) as Boolean) {
                return true
            }
            node = Bin(op = OP_DIV, left = b, right = a)
            if ((solve(run { val _tmp = rest.toMutableList(); _tmp.add(node); _tmp } as MutableList<Expr>)) as Boolean) {
                return true
            }
            j = j + 1
        }
        i = i + 1
    }
    return false
}

fun user_main(): Unit {
    var iter: Int = 0
    while (iter < 10) {
        var cards: MutableList<Expr> = mutableListOf()
        var i: Int = 0
        while (i < n_cards) {
            val n: Int = (_now() % (digit_range - 1)) + 1
            cards = run { val _tmp = cards.toMutableList(); _tmp.add(newNum(n)); _tmp } as MutableList<Expr>
            println(" " + n.toString())
            i = i + 1
        }
        println(":  ")
        if (!solve(cards)) {
            println("No solution")
        }
        iter = iter + 1
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
