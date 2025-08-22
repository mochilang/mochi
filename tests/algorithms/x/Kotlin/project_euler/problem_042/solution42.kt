val _dataDir = "/workspace/mochi/tests/github/TheAlgorithms/Mochi/project_euler/problem_042"

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun _ord(s: String): Int {
    return s[0].toInt()
}

fun _read_file(path: String): String {
    var f = java.io.File(path)
    if (!f.isAbsolute) {
        val local = if (_dataDir != "") java.io.File(_dataDir, path) else null
        if (local != null && local.exists()) {
            f = local
        } else {
            System.getenv("MOCHI_ROOT")?.let { root ->
                var clean = path
                while (clean.startsWith("../")) clean = clean.substring(3)
                var cand = java.io.File(root + "/tests/" + clean)
                if (!cand.exists()) cand = java.io.File(root + "/" + clean)
                f = cand
            }
        }
    }
    return if (f.exists()) f.readText() else ""
}

fun _numToStr(v: Number): String {
    val d = v.toDouble()
    val i = d.toLong()
    return if (d == i.toDouble()) i.toString() else d.toString()
}

fun triangular_numbers(limit: Int): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf<Int>()
    var n: Int = (1).toInt()
    while (n <= limit) {
        res = run { val _tmp = res.toMutableList(); _tmp.add((n * (n + 1)) / 2); _tmp }
        n = n + 1
    }
    return res
}

fun parse_words(text: String): MutableList<String> {
    var words: MutableList<String> = mutableListOf<String>()
    var current: String = ""
    var i: Int = (0).toInt()
    while (i < text.length) {
        var c: String = _sliceStr(text, i, i + 1)
        if (c == ",") {
            words = run { val _tmp = words.toMutableList(); _tmp.add(current); _tmp }
            current = ""
        } else {
            if (c == "\"") {
            } else {
                if ((c == "\r") || (c == "\n")) {
                } else {
                    current = current + c
                }
            }
        }
        i = i + 1
    }
    if (current.length > 0) {
        words = run { val _tmp = words.toMutableList(); _tmp.add(current); _tmp }
    }
    return words
}

fun word_value(word: String): Int {
    var total: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < word.length) {
        total = (total + _ord(_sliceStr(word, i, i + 1))) - 64
        i = i + 1
    }
    return total
}

fun contains(xs: MutableList<Int>, target: Int): Boolean {
    for (x in xs) {
        if (x == target) {
            return true
        }
    }
    return false
}

fun solution(): Int {
    var text: String = (_read_file("words.txt")) as String
    var words: MutableList<String> = parse_words(text)
    var tri: MutableList<Int> = triangular_numbers(100)
    var count: Int = (0).toInt()
    for (w in words) {
        var v: Int = (word_value(w)).toInt()
        if ((contains(tri, v)) as Boolean) {
            count = count + 1
        }
    }
    return count
}

fun main() {
    println(_numToStr(solution()))
}
