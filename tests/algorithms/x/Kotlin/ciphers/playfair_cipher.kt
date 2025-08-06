import java.math.BigInteger

fun contains(xs: MutableList<String>, x: String): Boolean {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return true
        }
        i = i + 1
    }
    return false
}

fun index_of(xs: MutableList<String>, x: String): Int {
    var i: Int = 0
    while (i < xs.size) {
        if (xs[i]!! == x) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun prepare_input(dirty: String): String {
    var letters: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    var upper_dirty: String = (dirty.toUpperCase() as String)
    var filtered: String = ""
    var i: Int = 0
    while (i < upper_dirty.length) {
        var c: String = upper_dirty.substring(i, i + 1)
        if (c in letters) {
            filtered = filtered + c
        }
        i = i + 1
    }
    if (filtered.length < 2) {
        return filtered
    }
    var clean: String = ""
    i = 0
    while (i < (filtered.length - 1)) {
        var c1: String = filtered.substring(i, i + 1)
        var c2: String = filtered.substring(i + 1, i + 2)
        clean = clean + c1
        if (c1 == c2) {
            clean = clean + "X"
        }
        i = i + 1
    }
    clean = clean + filtered.substring(filtered.length - 1, filtered.length)
    if ((Math.floorMod(clean.length, 2)) == 1) {
        clean = clean + "X"
    }
    return clean
}

fun generate_table(key: String): MutableList<String> {
    var alphabet: String = "ABCDEFGHIKLMNOPQRSTUVWXYZ"
    var table: MutableList<String> = mutableListOf<String>()
    var upper_key: String = (key.toUpperCase() as String)
    var i: Int = 0
    while (i < upper_key.length) {
        var c: String = upper_key.substring(i, i + 1)
        if (c in alphabet) {
            if (!((table.contains(c)) as Boolean)) {
                table = run { val _tmp = table.toMutableList(); _tmp.add(c); _tmp }
            }
        }
        i = i + 1
    }
    i = 0
    while (i < alphabet.length) {
        var c: String = alphabet.substring(i, i + 1)
        if (!((table.contains(c)) as Boolean)) {
            table = run { val _tmp = table.toMutableList(); _tmp.add(c); _tmp }
        }
        i = i + 1
    }
    return table
}

fun encode(plaintext: String, key: String): String {
    var table: MutableList<String> = generate_table(key)
    var text: String = prepare_input(plaintext)
    var cipher: String = ""
    var i: Int = 0
    while (i < text.length) {
        var c1: String = text.substring(i, i + 1)
        var c2: String = text.substring(i + 1, i + 2)
        var idx1: Int = index_of(table, c1)
        var idx2: Int = index_of(table, c2)
        var row1: Int = idx1 / 5
        var col1: Int = Math.floorMod(idx1, 5)
        var row2: Int = idx2 / 5
        var col2: Int = Math.floorMod(idx2, 5)
        if (row1 == row2) {
            cipher = cipher + table[(row1 * 5) + (Math.floorMod((col1 + 1), 5))]!!
            cipher = cipher + table[(row2 * 5) + (Math.floorMod((col2 + 1), 5))]!!
        } else {
            if (col1 == col2) {
                cipher = cipher + table[((Math.floorMod((row1 + 1), 5)) * 5) + col1]!!
                cipher = cipher + table[((Math.floorMod((row2 + 1), 5)) * 5) + col2]!!
            } else {
                cipher = cipher + table[(row1 * 5) + col2]!!
                cipher = cipher + table[(row2 * 5) + col1]!!
            }
        }
        i = i + 2
    }
    return cipher
}

fun decode(cipher: String, key: String): String {
    var table: MutableList<String> = generate_table(key)
    var plain: String = ""
    var i: Int = 0
    while (i < cipher.length) {
        var c1: String = cipher.substring(i, i + 1)
        var c2: String = cipher.substring(i + 1, i + 2)
        var idx1: Int = index_of(table, c1)
        var idx2: Int = index_of(table, c2)
        var row1: Int = idx1 / 5
        var col1: Int = Math.floorMod(idx1, 5)
        var row2: Int = idx2 / 5
        var col2: Int = Math.floorMod(idx2, 5)
        if (row1 == row2) {
            plain = plain + table[(row1 * 5) + (Math.floorMod((col1 + 4), 5))]!!
            plain = plain + table[(row2 * 5) + (Math.floorMod((col2 + 4), 5))]!!
        } else {
            if (col1 == col2) {
                plain = plain + table[((Math.floorMod((row1 + 4), 5)) * 5) + col1]!!
                plain = plain + table[((Math.floorMod((row2 + 4), 5)) * 5) + col2]!!
            } else {
                plain = plain + table[(row1 * 5) + col2]!!
                plain = plain + table[(row2 * 5) + col1]!!
            }
        }
        i = i + 2
    }
    return plain
}

fun user_main(): Unit {
    println(listOf("Encoded:", encode("BYE AND THANKS", "GREETING")).joinToString(" "))
    println(listOf("Decoded:", decode("CXRBANRLBALQ", "GREETING")).joinToString(" "))
}

fun main() {
    user_main()
}
