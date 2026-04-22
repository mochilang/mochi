data class Row(val dept: String, val name: String, val salary: Int)

fun main() {
    val toks = generateSequence { readLine() }.flatMap { it.trim().split(Regex("\\s+")).asSequence() }.filter { it.isNotEmpty() }.toList()
    if (toks.isEmpty()) return
    var idx = 0
    val t = toks[idx++].toInt()
    val cases = ArrayList<String>()
    repeat(t) {
        val d = toks[idx++].toInt()
        val e = toks[idx++].toInt()
        val deptName = HashMap<Int, String>()
        repeat(d) { deptName[toks[idx++].toInt()] = toks[idx++] }
        val groups = HashMap<Int, ArrayList<Pair<String, Int>>>()
        repeat(e) {
            idx++
            val name = toks[idx++]
            val salary = toks[idx++].toInt()
            val deptId = toks[idx++].toInt()
            groups.getOrPut(deptId) { arrayListOf() }.add(name to salary)
        }
        val rows = ArrayList<Row>()
        for ((deptId, items) in groups) {
            val keep = items.map { it.second }.toSet().sortedDescending().take(3).toSet()
            for ((name, salary) in items) if (salary in keep) rows.add(Row(deptName[deptId]!!, name, salary))
        }
        rows.sortWith(compareBy<Row> { it.dept }.thenByDescending { it.salary }.thenBy { it.name })
        cases.add((listOf(rows.size.toString()) + rows.map { "${it.dept},${it.name},${it.salary}" }).joinToString("\n"))
    }
    print(cases.joinToString("\n\n"))
}
