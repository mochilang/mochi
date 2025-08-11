import java.math.BigInteger

fun expect(cond: Boolean) { if (!cond) throw RuntimeException("expect failed") }

fun match_in_pattern(pat: String, ch: String): Int {
    var i: BigInteger = ((pat.length - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        if (pat.substring((i).toInt(), (i.add((1).toBigInteger())).toInt()) == ch) {
            return (i.toInt())
        }
        i = i.subtract((1).toBigInteger())
    }
    return 0 - 1
}

fun mismatch_in_text(text: String, pat: String, current_pos: Int): Int {
    var i: BigInteger = ((pat.length - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        if (pat.substring((i).toInt(), (i.add((1).toBigInteger())).toInt()) != text.substring(((current_pos).toBigInteger().add((i))).toInt(), (((current_pos).toBigInteger().add((i))).add((1).toBigInteger())).toInt())) {
            return (((current_pos).toBigInteger().add((i))).toInt())
        }
        i = i.subtract((1).toBigInteger())
    }
    return 0 - 1
}

fun bad_character_heuristic(text: String, pat: String): MutableList<Int> {
    var textLen: Int = (text.length).toInt()
    var patLen: Int = (pat.length).toInt()
    var positions: MutableList<Int> = mutableListOf<Int>()
    var i: Int = (0).toInt()
    while (i <= (textLen - patLen)) {
        var mismatch_index: Int = (mismatch_in_text(text, pat, i)).toInt()
        if (mismatch_index < 0) {
            positions = run { val _tmp = positions.toMutableList(); _tmp.add(i); _tmp }
            i = i + 1
        } else {
            var ch: String = text.substring(mismatch_index, mismatch_index + 1)
            var match_index: Int = (match_in_pattern(pat, ch)).toInt()
            if (match_index < 0) {
                i = mismatch_index + 1
            } else {
                i = mismatch_index - match_index
            }
        }
    }
    return positions
}

fun test_boyer_moore_basic(): Unit {
    var positions: MutableList<Int> = bad_character_heuristic("ABAABA", "AB")
    expect(positions == mutableListOf(0, 3))
}

fun test_boyer_moore_example(): Unit {
    var positions: MutableList<Int> = bad_character_heuristic("THIS IS A TEST TEXT", "TEST")
    expect(positions == mutableListOf(10))
}

fun main() {
    test_boyer_moore_basic()
    test_boyer_moore_example()
}
