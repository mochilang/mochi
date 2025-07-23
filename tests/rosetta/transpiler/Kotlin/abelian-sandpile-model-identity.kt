var s4: MutableList<Int> = mutableListOf(4, 3, 3, 3, 1, 2, 0, 2, 3)
var s1: MutableList<Int> = mutableListOf(1, 2, 0, 2, 1, 1, 0, 1, 3)
var s2: MutableList<Int> = mutableListOf(2, 1, 3, 1, 0, 1, 0, 1, 0)
var s3_a: MutableList<Int> = plus(s1, s2) as MutableList<Int>
var s3_b: MutableList<Int> = plus(s2, s1) as MutableList<Int>
var s3: MutableList<Int> = mutableListOf(3, 3, 3, 3, 3, 3, 3, 3, 3)
var s3_id: MutableList<Int> = mutableListOf(2, 1, 2, 1, 0, 1, 2, 1, 2)
var s4b: MutableList<Int> = plus(s3, s3_id) as MutableList<Int>
var s5: MutableList<Int> = plus(s3_id, s3_id) as MutableList<Int>
fun neighborsList(): MutableList<MutableList<Int>> {
    return mutableListOf(mutableListOf(1, 3), mutableListOf(0, 2, 4), mutableListOf(1, 5), mutableListOf(0, 4, 6), mutableListOf(1, 3, 5, 7), mutableListOf(2, 4, 8), mutableListOf(3, 7), mutableListOf(4, 6, 8), mutableListOf(5, 7)) as MutableList<MutableList<Int>>
}

fun plus(a: MutableList<Int>, b: MutableList<Int>): MutableList<Int> {
    var res: MutableList<Int> = mutableListOf()
    var i: Int = 0
    while (i < a.size) {
        res = run { val _tmp = res.toMutableList(); _tmp.add(a[i] + b[i]); _tmp } as MutableList<Int>
        i = i + 1
    }
    return res as MutableList<Int>
}

fun isStable(p: MutableList<Int>): Boolean {
    for (v in p) {
        if (v > 3) {
            return false as Boolean
        }
    }
    return true as Boolean
}

fun topple(p: MutableList<Int>): Int {
    val neighbors: MutableList<MutableList<Int>> = neighborsList() as MutableList<MutableList<Int>>
    var i: Int = 0
    while (i < p.size) {
        if (p[i] > 3) {
            p[i] = p[i] - 4
            val nbs = neighbors[i]
            for (j in nbs) {
                p[j] = (p)[j] as Int + 1
            }
            return 0 as Int
        }
        i = i + 1
    }
    return 0 as Int
}

fun pileString(p: MutableList<Int>): String {
    var s: String = ""
    var r: Int = 0
    while (r < 3) {
        var c: Int = 0
        while (c < 3) {
            s = (s + p[(3 * r) + c].toString()) + " "
            c = c + 1
        }
        s = s + "\n"
        r = r + 1
    }
    return s as String
}

fun main() {
    println("Avalanche of topplings:\n")
    println(pileString(s4) as String)
    while (!(isStable(s4) as Boolean)) {
        topple(s4) as Int
        println(pileString(s4) as String)
    }
    println("Commutative additions:\n")
    while (!(isStable(s3_a) as Boolean)) {
        topple(s3_a) as Int
    }
    while (!(isStable(s3_b) as Boolean)) {
        topple(s3_b) as Int
    }
    println((((pileString(s1) as String + "\nplus\n\n") + pileString(s2) as String) + "\nequals\n\n") + pileString(s3_a) as String)
    println((((("and\n\n" + pileString(s2) as String) + "\nplus\n\n") + pileString(s1) as String) + "\nalso equals\n\n") + pileString(s3_b) as String)
    println("Addition of identity sandpile:\n")
    while (!(isStable(s4b) as Boolean)) {
        topple(s4b) as Int
    }
    println((((pileString(s3) as String + "\nplus\n\n") + pileString(s3_id) as String) + "\nequals\n\n") + pileString(s4b) as String)
    println("Addition of identities:\n")
    while (!(isStable(s5) as Boolean)) {
        topple(s5) as Int
    }
    println((((pileString(s3_id) as String + "\nplus\n\n") + pileString(s3_id) as String) + "\nequals\n\n") + pileString(s5) as String)
}
