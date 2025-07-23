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

var adfgvx: String = "ADFGVX"
var alphabet: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
fun shuffleStr(s: String): String {
    var arr: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < s.length) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(s.substring(i, i + 1)); _tmp } as MutableList<String>
        i = i + 1
    }
    var j: Int = arr.size - 1
    while (j > 0) {
        val k: Int = _now() % (j + 1)
        val tmp: String = arr[j]
        arr[j] = arr[k]
        arr[k] = tmp
        j = j - 1
    }
    var out: String = ""
    i = 0
    while (i < arr.size) {
        out = out + arr[i]
        i = i + 1
    }
    return out
}

fun createPolybius(): MutableList<String> {
    val shuffled: String = shuffleStr(alphabet) as String
    var labels: MutableList<String> = mutableListOf()
    var li: Int = 0
    while (li < adfgvx.length) {
        labels = run { val _tmp = labels.toMutableList(); _tmp.add(adfgvx.substring(li, li + 1)); _tmp } as MutableList<String>
        li = li + 1
    }
    println("6 x 6 Polybius square:\n")
    println("  | A D F G V X")
    println("---------------")
    var p: MutableList<String> = mutableListOf()
    var i: Int = 0
    while (i < 6) {
        var row: String = shuffled.substring(i * 6, (i + 1) * 6)
        p = run { val _tmp = p.toMutableList(); _tmp.add(row); _tmp } as MutableList<String>
        var line: String = labels[i] + " | "
        var j: Int = 0
        while (j < 6) {
            line = (line + row.substring(j, j + 1)) + " "
            j = j + 1
        }
        println(line)
        i = i + 1
    }
    return p
}

fun createKey(n: Int): String {
    if ((n < 7) || (n > 12)) {
        println("Key should be within 7 and 12 letters long.")
    }
    var pool: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    var key: String = ""
    var i: Int = 0
    while (i < n) {
        val idx: Int = _now() % pool.length
        key = key + (pool[idx]).toString()
        pool = pool.substring(0, idx) + pool.substring(idx + 1, pool.length)
        i = i + 1
    }
    println("\nThe key is " + key)
    return key
}

fun orderKey(key: String): MutableList<Int> {
    var pairs: MutableList<Any> = mutableListOf()
    var i: Int = 0
    while (i < key.length) {
        pairs = run { val _tmp = pairs.toMutableList(); _tmp.add(mutableListOf(key.substring(i, i + 1), i)); _tmp }
        i = i + 1
    }
    var n: Int = pairs.size
    var m: Int = 0
    while (m < n) {
        var j: Int = 0
        while (j < (n - 1)) {
            if (pairs[j][0] > pairs[j + 1][0]) {
                val tmp = pairs[j]
                pairs[j] = pairs[j + 1]
                pairs[j + 1] = tmp
            }
            j = j + 1
        }
        m = m + 1
    }
    var res: MutableList<Any> = mutableListOf()
    i = 0
    while (i < n) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(pairs[i][1].toInt()); _tmp }
        i = i + 1
    }
    return res as MutableList<Int>
}

fun encrypt(polybius: MutableList<String>, key: String, plainText: String): String {
    var labels: MutableList<String> = mutableListOf()
    var li: Int = 0
    while (li < adfgvx.length) {
        labels = run { val _tmp = labels.toMutableList(); _tmp.add(adfgvx.substring(li, li + 1)); _tmp } as MutableList<String>
        li = li + 1
    }
    var temp: String = ""
    var i: Int = 0
    while (i < plainText.length) {
        var r: Int = 0
        while (r < 6) {
            var c: Int = 0
            while (c < 6) {
                if (polybius[r].subList(c, c + 1) == plainText.substring(i, i + 1)) {
                    temp = (((temp + (labels.subList(r, r + 1)).toString()).toMutableList()) + labels.subList(c, c + 1)).toMutableList() as String
                }
                c = c + 1
            }
            r = r + 1
        }
        i = i + 1
    }
    var colLen: Int = temp.length / key.length
    if ((temp.length % key.length) > 0) {
        colLen = colLen + 1
    }
    var table: MutableList<MutableList<String>> = mutableListOf()
    var rIdx: Int = 0
    while (rIdx < colLen) {
        var row: MutableList<String> = mutableListOf()
        var j: Int = 0
        while (j < key.length) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(""); _tmp } as MutableList<String>
            j = j + 1
        }
        table = run { val _tmp = table.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<String>>
        rIdx = rIdx + 1
    }
    var idx: Int = 0
    while (idx < temp.length) {
        val row: MutableList<String> = idx / key.length
        val col: Int = idx % key.length
        (table)[row] as MutableList<String>[col] = temp.substring(idx, idx + 1)
        idx = idx + 1
    }
    val order: MutableList<Int> = orderKey(key) as MutableList<Int>
    var cols: MutableList<String> = mutableListOf()
    var ci: Int = 0
    while (ci < key.length) {
        var colStr: String = ""
        var ri: Int = 0
        while (ri < colLen) {
            colStr = colStr + table[ri][order[ci]]
            ri = ri + 1
        }
        cols = run { val _tmp = cols.toMutableList(); _tmp.add(colStr); _tmp } as MutableList<String>
        ci = ci + 1
    }
    var result: String = ""
    ci = 0
    while (ci < cols.size) {
        result = result + cols[ci]
        if (ci < (cols.size - 1)) {
            result = result + " "
        }
        ci = ci + 1
    }
    return result
}

fun indexOf(s: String, ch: String): Int {
    var i: Int = 0
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun decrypt(polybius: MutableList<String>, key: String, cipherText: String): String {
    var colStrs: MutableList<String> = mutableListOf()
    var start: Int = 0
    var i: Int = 0
    while (i <= cipherText.length) {
        if ((i == cipherText.length) || (cipherText[i] == " ")) {
            colStrs = run { val _tmp = colStrs.toMutableList(); _tmp.add(cipherText.substring(start, i)); _tmp } as MutableList<String>
            start = i + 1
        }
        i = i + 1
    }
    var maxColLen: Int = 0
    i = 0
    while (i < colStrs.size) {
        if (colStrs[i].length > maxColLen) {
            maxColLen = colStrs[i].length
        }
        i = i + 1
    }
    var cols: MutableList<MutableList<String>> = mutableListOf()
    i = 0
    while (i < colStrs.size) {
        var s: String = colStrs[i]
        var ls: MutableList<String> = mutableListOf()
        var j: Int = 0
        while (j < s.length) {
            ls = run { val _tmp = ls.toMutableList(); _tmp.add(s.substring(j, j + 1)); _tmp } as MutableList<String>
            j = j + 1
        }
        if (s.length < maxColLen) {
            var pad: MutableList<String> = mutableListOf()
            var k: Int = 0
            while (k < maxColLen) {
                if (k < ls.size) {
                    pad = run { val _tmp = pad.toMutableList(); _tmp.add(ls[k]); _tmp } as MutableList<String>
                } else {
                    pad = run { val _tmp = pad.toMutableList(); _tmp.add(""); _tmp } as MutableList<String>
                }
                k = k + 1
            }
            cols = run { val _tmp = cols.toMutableList(); _tmp.add(pad); _tmp } as MutableList<MutableList<String>>
        } else {
            cols = run { val _tmp = cols.toMutableList(); _tmp.add(ls); _tmp } as MutableList<MutableList<String>>
        }
        i = i + 1
    }
    var table: MutableList<MutableList<String>> = mutableListOf()
    var r: Int = 0
    while (r < maxColLen) {
        var row: MutableList<String> = mutableListOf()
        var c: Int = 0
        while (c < key.length) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(""); _tmp } as MutableList<String>
            c = c + 1
        }
        table = run { val _tmp = table.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<String>>
        r = r + 1
    }
    val order: MutableList<Int> = orderKey(key) as MutableList<Int>
    r = 0
    while (r < maxColLen) {
        var c: Int = 0
        while (c < key.length) {
            table[r][order[c]] = cols[c][r]
            c = c + 1
        }
        r = r + 1
    }
    var temp: String = ""
    r = 0
    while (r < table.size) {
        var j: Int = 0
        while (j < table[r].size) {
            temp = temp + table[r][j]
            j = j + 1
        }
        r = r + 1
    }
    var plainText: String = ""
    var idx: Int = 0
    while (idx < temp.length) {
        val rIdx: Int = indexOf(adfgvx, temp.substring(idx, idx + 1)) as Int
        val cIdx: Int = indexOf(adfgvx, temp.substring(idx + 1, idx + 2)) as Int
        plainText = plainText + (polybius[rIdx][cIdx]).toString()
        idx = idx + 2
    }
    return plainText
}

fun user_main(): Unit {
    val plainText: String = "ATTACKAT1200AM"
    val polybius: MutableList<String> = createPolybius() as MutableList<String>
    val key: String = createKey(9) as String
    println("\nPlaintext : " + plainText)
    val cipherText: String = encrypt(polybius, key, plainText) as String
    println("\nEncrypted : " + cipherText)
    val plainText2: String = decrypt(polybius, key, cipherText) as String
    println("\nDecrypted : " + plainText2)
}

fun main() {
    user_main()
}
