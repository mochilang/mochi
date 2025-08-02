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

fun padLeft(s: String, w: Int): String {
    var res: String = ""
    var n: BigInteger = (w - s.length).toBigInteger()
    while (n.compareTo((0).toBigInteger()) > 0) {
        res = res + " "
        n = n.subtract((1).toBigInteger())
    }
    return res + s
}

fun indexOfFrom(s: String, ch: String, start: Int): Int {
    var i: Int = start
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun containsStr(s: String, sub: String): Boolean {
    var i: Int = 0
    var sl: Int = s.length
    var subl: Int = sub.length
    while (i <= (sl - subl)) {
        if (s.substring(i, i + subl) == sub) {
            return true
        }
        i = i + 1
    }
    return false
}

fun distinct(slist: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    for (s in slist) {
        var found: Boolean = false
        for (r in res) {
            if (r == s) {
                found = true
                break
            }
        }
        if (!found) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(s); _tmp } as MutableList<String>
        }
    }
    return res
}

fun permutations(xs: MutableList<String>): MutableList<MutableList<String>> {
    if (xs.size <= 1) {
        return mutableListOf(xs)
    }
    var res: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
    var i: Int = 0
    while (i < xs.size) {
        var rest: MutableList<String> = mutableListOf<String>()
        var j: Int = 0
        while (j < xs.size) {
            if (j != i) {
                rest = run { val _tmp = rest.toMutableList(); _tmp.add(xs[j]!!); _tmp } as MutableList<String>
            }
            j = j + 1
        }
        var subs: MutableList<MutableList<String>> = permutations(rest)
        for (p in subs) {
            var perm: MutableList<String> = mutableListOf(xs[i]!!)
            var k: Int = 0
            while (k < p.size) {
                perm = run { val _tmp = perm.toMutableList(); _tmp.add(p[k]!!); _tmp } as MutableList<String>
                k = k + 1
            }
            res = run { val _tmp = res.toMutableList(); _tmp.add(perm); _tmp } as MutableList<MutableList<String>>
        }
        i = i + 1
    }
    return res
}

fun headTailOverlap(s1: String, s2: String): Int {
    var start: Int = 0
    while (true) {
        var ix: Int = indexOfFrom(s1, s2.substring(0, 1), start)
        if (ix == (0 - 1)) {
            return 0
        }
        start = ix
        var sublen: BigInteger = (s1.length - start).toBigInteger()
        if (sublen.compareTo((s2.length).toBigInteger()) > 0) {
            sublen = s2.length.toBigInteger()
        }
        if (s2.substring(0, (sublen).toInt()) == s1.substring(start, ((start).toBigInteger().add((sublen))).toInt())) {
            return sublen.toInt()
        }
        start = start + 1
    }
}

fun deduplicate(slist: MutableList<String>): MutableList<String> {
    var arr = distinct(slist)
    var filtered: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < arr.size) {
        var s1: Any? = arr[i] as Any?
        var within: Boolean = false
        var j: Int = 0
        while (j < arr.size) {
            if ((j != i) && containsStr((arr[j] as Any?) as String, s1 as String)) {
                within = true
                break
            }
            j = j + 1
        }
        if (!within) {
            filtered = run { val _tmp = filtered.toMutableList(); _tmp.add(s1 as String); _tmp } as MutableList<String>
        }
        i = i + 1
    }
    return filtered
}

fun joinAll(ss: MutableList<String>): String {
    var out: String = ""
    for (s in ss) {
        out = out + s
    }
    return out
}

fun shortestCommonSuperstring(slist: MutableList<String>): String {
    var ss: MutableList<String> = deduplicate(slist)
    var shortest: String = joinAll(ss)
    var perms: MutableList<MutableList<String>> = permutations(ss)
    var idx: Int = 0
    while (idx < perms.size) {
        var perm: MutableList<String> = perms[idx]!!
        var sup: String = perm[0]!!
        var i: Int = 0
        while (i < (ss.size - 1)) {
            var ov: Int = headTailOverlap(perm[i]!!, perm[i + 1]!!)
            sup = sup + perm[i + 1]!!.substring(ov, (perm[i + 1]!!).length)
            i = i + 1
        }
        if (sup.length < shortest.length) {
            shortest = sup
        }
        idx = idx + 1
    }
    return shortest
}

fun printCounts(seq: String): Unit {
    var a: Int = 0
    var c: Int = 0
    var g: Int = 0
    var t: Int = 0
    var i: Int = 0
    while (i < seq.length) {
        var ch: String = seq.substring(i, i + 1)
        if (ch == "A") {
            a = a + 1
        } else {
            if (ch == "C") {
                c = c + 1
            } else {
                if (ch == "G") {
                    g = g + 1
                } else {
                    if (ch == "T") {
                        t = t + 1
                    }
                }
            }
        }
        i = i + 1
    }
    var total: Int = seq.length
    println(("\nNucleotide counts for " + seq) + ":\n")
    println(padLeft("A", 10) + padLeft(a.toString(), 12))
    println(padLeft("C", 10) + padLeft(c.toString(), 12))
    println(padLeft("G", 10) + padLeft(g.toString(), 12))
    println(padLeft("T", 10) + padLeft(t.toString(), 12))
    println(padLeft("Other", 10) + padLeft((total - (((a + c) + g) + t)).toString(), 12))
    println("  ____________________")
    println(padLeft("Total length", 14) + padLeft(total.toString(), 8))
}

fun user_main(): Unit {
    var tests: MutableList<MutableList<String>> = mutableListOf(mutableListOf("TA", "AAG", "TA", "GAA", "TA"), mutableListOf("CATTAGGG", "ATTAG", "GGG", "TA"), mutableListOf("AAGAUGGA", "GGAGCGCAUC", "AUCGCAAUAAGGA"), mutableListOf("ATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTAT", "GGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGT", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "AACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTT", "GCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTC", "CGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATTCTGCTTATAACACTATGTTCT", "TGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CGTAAAAAATTACAACGTCCTTTGGCTATCTCTTAAACTCCTGCTAAATGCTCGTGC", "GATGGAGCGCATCGAACGCAATAAGGATCATTTGATGGGACGTTTCGTCGACAAAGTCTTGTTTCGAGAGTAACGGCTACCGTCTTCGATT", "TTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGATGGAGCGCATC", "CTATGTTCTTATGAAATGGATGTTCTGAGTTGGTCAGTCCCAATGTGCGGGGTTTCTTTTAGTACGTCGGGAGTGGTATTATA", "TCTCTTAAACTCCTGCTAAATGCTCGTGCTTTCCAATTATGTAAGCGTTCCGAGACGGGGTGGTCGATTCTGAGGACAAAGGTCAAGA"))
    for (seqs in tests) {
        var scs: String = shortestCommonSuperstring(seqs)
        printCounts(scs)
    }
}

fun main() {
    run {
        System.gc()
        val _startMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _start = _now()
        user_main()
        System.gc()
        val _end = _now()
        val _endMem = Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
        val _durationUs = (_end - _start) / 1000
        val _memDiff = kotlin.math.abs(_endMem - _startMem)
        val _res = mapOf("duration_us" to _durationUs, "memory_bytes" to _memDiff, "name" to "main")
        println(toJson(_res))
    }
}
