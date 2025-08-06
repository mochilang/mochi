import java.math.BigInteger

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

var KEY_STRING: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
var key: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(2, 5), mutableListOf(1, 6))
fun mod36(n: Int): Int {
    var r: BigInteger = ((Math.floorMod(n, 36)).toBigInteger())
    if (r.compareTo((0).toBigInteger()) < 0) {
        r = r.add((36).toBigInteger())
    }
    return (r.toInt())
}

fun gcd(a: Int, b: Int): Int {
    var x: Int = a
    var y: Int = b
    while (y != 0) {
        var t: Int = y
        y = Math.floorMod(x, y)
        x = t
    }
    if (x < 0) {
        x = 0 - x
    }
    return x
}

fun replace_letters(letter: String): Int {
    var i: Int = 0
    while (i < KEY_STRING.length) {
        if (KEY_STRING[i].toString() == letter) {
            return i
        }
        i = i + 1
    }
    return 0
}

fun replace_digits(num: Int): String {
    var idx: Int = mod36(num)
    return KEY_STRING[idx].toString()
}

fun to_upper(c: String): String {
    var lower: String = "abcdefghijklmnopqrstuvwxyz"
    var upper: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var i: Int = 0
    while (i < lower.length) {
        if (c == lower[i].toString()) {
            return upper[i].toString()
        }
        i = i + 1
    }
    return c
}

fun process_text(text: String, break_key: Int): String {
    var chars: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < text.length) {
        var c: String = to_upper(text[i].toString())
        var j: Int = 0
        var ok: Boolean = false
        while (j < KEY_STRING.length) {
            if (KEY_STRING[j].toString() == c) {
                ok = true
                break
            }
            j = j + 1
        }
        if ((ok as Boolean)) {
            chars = run { val _tmp = chars.toMutableList(); _tmp.add(c); _tmp }
        }
        i = i + 1
    }
    if (chars.size == 0) {
        return ""
    }
    var last: String = chars[chars.size - 1]!!
    while ((Math.floorMod(chars.size, break_key)) != 0) {
        chars = run { val _tmp = chars.toMutableList(); _tmp.add(last); _tmp }
    }
    var res: String = ""
    var k: Int = 0
    while (k < chars.size) {
        res = res + chars[k]!!
        k = k + 1
    }
    return res
}

fun matrix_minor(m: MutableList<MutableList<Int>>, row: Int, col: Int): MutableList<MutableList<Int>> {
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < m.size) {
        if (i != row) {
            var r: MutableList<Int> = mutableListOf<Int>()
            var j: Int = 0
            while (j < (m[i]!!).size) {
                if (j != col) {
                    r = run { val _tmp = r.toMutableList(); _tmp.add((((m[i]!!) as MutableList<Int>))[j]!!); _tmp }
                }
                j = j + 1
            }
            res = run { val _tmp = res.toMutableList(); _tmp.add(r); _tmp }
        }
        i = i + 1
    }
    return res
}

fun determinant(m: MutableList<MutableList<Int>>): Int {
    var n: Int = m.size
    if (n == 1) {
        return (((m[0]!!) as MutableList<Int>))[0]!!
    }
    if (n == 2) {
        return ((((m[0]!!) as MutableList<Int>))[0]!! * (((m[1]!!) as MutableList<Int>))[1]!!) - ((((m[0]!!) as MutableList<Int>))[1]!! * (((m[1]!!) as MutableList<Int>))[0]!!)
    }
    var det: Int = 0
    var col: Int = 0
    while (col < n) {
        var minor_mat: MutableList<MutableList<Int>> = matrix_minor(m, 0, col)
        var sign: Int = 1
        if ((Math.floorMod(col, 2)) == 1) {
            sign = 0 - 1
        }
        det = det + ((sign * (((m[0]!!) as MutableList<Int>))[col]!!) * determinant(minor_mat))
        col = col + 1
    }
    return det
}

fun cofactor_matrix(m: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var n: Int = m.size
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            var minor_mat: MutableList<MutableList<Int>> = matrix_minor(m, i, j)
            var det_minor: Int = determinant(minor_mat)
            var sign: Int = 1
            if ((Math.floorMod((i + j), 2)) == 1) {
                sign = 0 - 1
            }
            row = run { val _tmp = row.toMutableList(); _tmp.add(sign * det_minor); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun transpose(m: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var rows: Int = m.size
    var cols: Int = (m[0]!!).size
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var j: Int = 0
    while (j < cols) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var i: Int = 0
        while (i < rows) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((m[i]!!) as MutableList<Int>))[j]!!); _tmp }
            i = i + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        j = j + 1
    }
    return res
}

fun matrix_mod(m: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < m.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < (m[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(mod36((((m[i]!!) as MutableList<Int>))[j]!!)); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun scalar_matrix_mult(s: Int, m: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < m.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < (m[i]!!).size) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(mod36(s * (((m[i]!!) as MutableList<Int>))[j]!!)); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun adjugate(m: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var cof: MutableList<MutableList<Int>> = cofactor_matrix(m)
    var n: Int = cof.size
    var res: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = 0
    while (i < n) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < n) {
            row = run { val _tmp = row.toMutableList(); _tmp.add((((cof[j]!!) as MutableList<Int>))[i]!!); _tmp }
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return res
}

fun multiply_matrix_vector(m: MutableList<MutableList<Int>>, v: MutableList<Int>): MutableList<Int> {
    var n: Int = m.size
    var res: MutableList<Int> = mutableListOf<Int>()
    var i: Int = 0
    while (i < n) {
        var sum: Int = 0
        var j: Int = 0
        while (j < n) {
            sum = sum + ((((m[i]!!) as MutableList<Int>))[j]!! * v[j]!!)
            j = j + 1
        }
        res = run { val _tmp = res.toMutableList(); _tmp.add(mod36(sum)); _tmp }
        i = i + 1
    }
    return res
}

fun inverse_key(key: MutableList<MutableList<Int>>): MutableList<MutableList<Int>> {
    var det_val: Int = determinant(key)
    var det_mod: Int = mod36(det_val)
    var det_inv: Int = 0
    var i: Int = 0
    while (i < 36) {
        if ((Math.floorMod((det_mod * i), 36)) == 1) {
            det_inv = i
            break
        }
        i = i + 1
    }
    var adj: MutableList<MutableList<Int>> = adjugate(key)
    var tmp: MutableList<MutableList<Int>> = scalar_matrix_mult(det_inv, adj)
    var res: MutableList<MutableList<Int>> = matrix_mod(tmp)
    return res
}

fun hill_encrypt(key: MutableList<MutableList<Int>>, text: String): String {
    var break_key: Int = key.size
    var processed: String = process_text(text, break_key)
    var encrypted: String = ""
    var i: Int = 0
    while (i < processed.length) {
        var vec: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < break_key) {
            vec = run { val _tmp = vec.toMutableList(); _tmp.add(replace_letters(processed[i + j].toString())); _tmp }
            j = j + 1
        }
        var enc_vec: MutableList<Int> = multiply_matrix_vector(key, vec)
        var k: Int = 0
        while (k < break_key) {
            encrypted = encrypted + replace_digits(enc_vec[k]!!)
            k = k + 1
        }
        i = i + break_key
    }
    return encrypted
}

fun hill_decrypt(key: MutableList<MutableList<Int>>, text: String): String {
    var break_key: Int = key.size
    var decrypt_key: MutableList<MutableList<Int>> = inverse_key(key)
    var processed: String = process_text(text, break_key)
    var decrypted: String = ""
    var i: Int = 0
    while (i < processed.length) {
        var vec: MutableList<Int> = mutableListOf<Int>()
        var j: Int = 0
        while (j < break_key) {
            vec = run { val _tmp = vec.toMutableList(); _tmp.add(replace_letters(processed[i + j].toString())); _tmp }
            j = j + 1
        }
        var dec_vec: MutableList<Int> = multiply_matrix_vector(decrypt_key, vec)
        var k: Int = 0
        while (k < break_key) {
            decrypted = decrypted + replace_digits(dec_vec[k]!!)
            k = k + 1
        }
        i = i + break_key
    }
    return decrypted
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        println(hill_encrypt(key, "testing hill cipher"))
        println(hill_encrypt(key, "hello"))
        println(hill_decrypt(key, "WHXYJOLM9C6XT085LL"))
        println(hill_decrypt(key, "85FF00"))
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
