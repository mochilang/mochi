import java.math.BigInteger

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun allConstruct(target: String, wordBank: MutableList<String>): MutableList<MutableList<String>> {
    var tableSize: Int = (target.length + 1).toInt()
    var table: MutableList<MutableList<MutableList<String>>> = mutableListOf<MutableList<MutableList<String>>>()
    var idx: Int = (0).toInt()
    while (idx < tableSize) {
        var empty: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
        table = run { val _tmp = table.toMutableList(); _tmp.add(empty); _tmp }
        idx = idx + 1
    }
    var base: MutableList<String> = mutableListOf<String>()
    _listSet(table, 0, mutableListOf(base))
    var i: Int = (0).toInt()
    while (i < tableSize) {
        if ((table[i]!!).size != 0) {
            var w: Int = (0).toInt()
            while (w < wordBank.size) {
                var word: String = wordBank[w]!!
                var wordLen: Int = (word.length).toInt()
                if (_sliceStr(target, i, i + wordLen) == word) {
                    var k: Int = (0).toInt()
                    while (k < (table[i]!!).size) {
                        var way: MutableList<String> = (((table[i]!!) as MutableList<MutableList<String>>))[k]!!
                        var combination: MutableList<String> = mutableListOf<String>()
                        var m: Int = (0).toInt()
                        while (m < way.size) {
                            combination = run { val _tmp = combination.toMutableList(); _tmp.add(way[m]!!); _tmp }
                            m = m + 1
                        }
                        combination = run { val _tmp = combination.toMutableList(); _tmp.add(word); _tmp }
                        var nextIndex: Int = (i + wordLen).toInt()
                        _listSet(table, nextIndex, run { val _tmp = (table[nextIndex]!!).toMutableList(); _tmp.add(combination); _tmp })
                        k = k + 1
                    }
                }
                w = w + 1
            }
        }
        i = i + 1
    }
    return table[target.length]!!
}

fun main() {
    println(allConstruct("jwajalapa", mutableListOf("jwa", "j", "w", "a", "la", "lapa")).toString())
    println(allConstruct("rajamati", mutableListOf("s", "raj", "amat", "raja", "ma", "i", "t")).toString())
    println(allConstruct("hexagonosaurus", mutableListOf("h", "ex", "hex", "ag", "ago", "ru", "auru", "rus", "go", "no", "o", "s")).toString())
}
