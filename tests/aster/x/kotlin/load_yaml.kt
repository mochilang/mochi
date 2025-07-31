fun _load(path: user_type, opts: user_type): MutableList<MutableMap<String, user_type>> {
    val fmt = as_expression ?: "csv"
    val lines = if_expression
    return when_expression
}

fun loadYamlSimple(lines: List<String>): MutableList<MutableMap<String, user_type>> {
    val res = mutableListOftype_arguments
    val cur: user_type = null
    for (ln in lines) {
        val t = ln.trim()
        if (t.startsWith("- ")) {
            cur.let {res.add(it)}
            cur = mutableMapOf()
            val idx = t.indexOf(character_literal, 2)
            if (idx >= 0) {
                val k = t.substring(2, idx).trim()
                val v = parseSimpleValue(t.substring(idx + 1))
                !cur[k] = v
            }
        }
    }
    cur.let {res.add(it)}
    return res
}

fun parseSimpleValue(s: String) nullable_type
{
    val t = s.trim()
    return when_expression
}

data class Person(val name: String, val age: Int, val email: String)

fun main() {
    val people: MutableList<Person> = _load("../interpreter/valid/people.yaml", mutableMapOf < String, String > ("format" to "yaml")).map({it -> Person(name = (as_expression), age = (as_expression), email = (as_expression))}).toMutableList()
    val adults: MutableList<MutableMap<String, String>> = run {
        val _res = mutableListOftype_arguments
        for (p in people) {
            if (p.age >= 18) {
                _res.add(mutableMapOftype_arguments)
            }
        }
        _res
    }
    for (a in adults) {
        println(listOf((!a["name"]), (!a["email"])).joinToString(" "))
    }
}
