fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

var _nowSeed = 0L
var _nowSeeded = false
fun _now(): Long {
    if (!_nowSeeded) {
        System.getenv("MOCHI_NOW_SEED")?.toLongOrNull()?.let {
            _nowSeed = it
            _nowSeeded = true
        }
    }
    return if (_nowSeeded) {
        _nowSeed = (_nowSeed * 1664525 + 1013904223) % 2147483647
        kotlin.math.abs(_nowSeed)
    } else {
        kotlin.math.abs(System.nanoTime())
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

data class Clause(var literals: MutableMap<String, Int> = mutableMapOf<String, Int>(), var names: MutableList<String> = mutableListOf<String>())
data class EvalResult(var value: Int = 0, var clause: Clause = Clause(literals = mutableMapOf<String, Int>(), names = mutableListOf<String>()))
data class Formula(var clauses: MutableList<Clause> = mutableListOf<Clause>())
data class DPLLResult(var sat: Boolean = false, var model: MutableMap<String, Int> = mutableMapOf<String, Int>())
var clause1: Clause = new_clause(mutableListOf("A4", "A3", "A5'", "A1", "A3'"))
var clause2: Clause = new_clause(mutableListOf("A4"))
var formula: Formula = new_formula(mutableListOf(clause1, clause2))
var formula_str: String = str_formula(formula)
var clauses: MutableList<Clause> = mutableListOf(clause1, clause2)
var symbols: MutableList<String> = mutableListOf("A4", "A3", "A5", "A1")
var model: MutableMap<String, Int> = mutableMapOf<String, Int>()
var result: DPLLResult = dpll_algorithm(clauses, symbols, model)
fun new_clause(lits: MutableList<String>): Clause {
    var m: MutableMap<String, Int> = mutableMapOf<String, Int>()
    var names: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < lits.size) {
        var lit: String = lits[i]!!
        (m)[lit] = 0 - 1
        names = run { val _tmp = names.toMutableList(); _tmp.add(lit); _tmp }
        i = i + 1
    }
    return Clause(literals = m, names = names)
}

fun assign_clause(c: Clause, model: MutableMap<String, Int>): Clause {
    var lits: MutableMap<String, Int> = c.literals
    var i: Int = (0).toInt()
    while (i < (c.names).size) {
        var lit: String = (c.names)[i]!!
        var symbol: String = lit.substring(0, 2)
        if (symbol in model) {
            var value: Int? = (((model)[symbol] as Int).toInt())
            if ((lit.substring(lit.length - 1, lit.length) == "'") && (value != (0 - 1))) {
                value = 1 - value
            }
            (lits)[lit] = value
        }
        i = i + 1
    }
    c.literals = lits
    return c
}

fun evaluate_clause(c: Clause, model: MutableMap<String, Int>): EvalResult {
    var c: Clause = c
    var i: Int = (0).toInt()
    while (i < (c.names).size) {
        var lit: String = (c.names)[i]!!
        var sym: String = (if (lit.substring(lit.length - 1, lit.length) == "'") lit.substring(0, 2) else lit + "'" as String)
        if (sym in c.literals) {
            return EvalResult(value = 1, clause = c)
        }
        i = i + 1
    }
    c = assign_clause(c, model)
    i = 0
    while (i < (c.names).size) {
        var lit: String = (c.names)[i]!!
        var value: Int = ((c.literals)[lit] as Int).toInt()
        if (value == 1) {
            return EvalResult(value = 1, clause = c)
        }
        if (value == (0 - 1)) {
            return EvalResult(value = 0 - 1, clause = c)
        }
        i = i + 1
    }
    var any_true: Int = (0).toInt()
    i = 0
    while (i < (c.names).size) {
        var lit: String = (c.names)[i]!!
        if ((c.literals)[lit] as Int == 1) {
            any_true = 1
        }
        i = i + 1
    }
    return EvalResult(value = any_true, clause = c)
}

fun new_formula(cs: MutableList<Clause>): Formula {
    return Formula(clauses = cs)
}

fun remove_symbol(symbols: MutableList<String>, s: String): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    var i: Int = (0).toInt()
    while (i < symbols.size) {
        if (symbols[i]!! != s) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(symbols[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun dpll_algorithm(clauses: MutableList<Clause>, symbols: MutableList<String>, model: MutableMap<String, Int>): DPLLResult {
    var all_true: Boolean = true
    var i: Int = (0).toInt()
    while (i < clauses.size) {
        var ev: EvalResult = evaluate_clause(clauses[i]!!, model)
        _listSet(clauses, i, ev.clause)
        if (ev.value == 0) {
            return DPLLResult(sat = false, model = mutableMapOf<String, Int>())
        } else {
            if (ev.value == (0 - 1)) {
                all_true = false
            }
        }
        i = i + 1
    }
    if ((all_true as Boolean)) {
        return DPLLResult(sat = true, model = model)
    }
    var p: String = symbols[0]!!
    var rest: MutableList<String> = remove_symbol(symbols, p)
    var tmp1: MutableMap<String, Int> = model
    var tmp2: MutableMap<String, Int> = model
    (tmp1)[p] = 1
    (tmp2)[p] = 0
    var res1: DPLLResult = dpll_algorithm(clauses, rest, tmp1)
    if (((res1.sat) as Boolean)) {
        return res1
    }
    return dpll_algorithm(clauses, rest, tmp2)
}

fun str_clause(c: Clause): String {
    var line: String = "{"
    var first: Boolean = true
    var i: Int = (0).toInt()
    while (i < (c.names).size) {
        var lit: String = (c.names)[i]!!
        if ((first as Boolean)) {
            first = false
        } else {
            line = line + " , "
        }
        line = line + lit
        i = i + 1
    }
    line = line + "}"
    return line
}

fun str_formula(f: Formula): String {
    var line: String = "{"
    var i: Int = (0).toInt()
    while (i < (f.clauses).size) {
        line = line + str_clause((f.clauses)[i]!!)
        if (i < ((f.clauses).size - 1)) {
            line = line + " , "
        }
        i = i + 1
    }
    line = line + "}"
    return line
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        if (((result.sat) as Boolean)) {
            println(("The formula " + formula_str) + " is satisfiable.")
        } else {
            println(("The formula " + formula_str) + " is not satisfiable.")
        }
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
