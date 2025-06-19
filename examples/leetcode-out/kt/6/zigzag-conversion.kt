fun convert(s: String, numRows: Int) : String {
        if (((numRows <= 1) || (numRows >= s.length))) {
                return s
        }
        var rows: List<String> = listOf()
        var i = 0
        while ((i < numRows)) {
                rows = (rows + listOf(""))
                i = (i + 1)
        }
        var curr = 0
        var step = 1
        for (ch in s) {
                run {
                        val _tmp = rows.toMutableList()
                        _tmp[curr] = (rows[curr] + ch)
                        rows = _tmp
                }
                if ((curr == 0)) {
                        step = 1
                } else if ((curr == (numRows - 1))) {
                        step = -1
                }
                curr = (curr + step)
        }
        var result: String = ""
        for (row in rows) {
                result = (result + row)
        }
        return result
}

fun main() {
}

