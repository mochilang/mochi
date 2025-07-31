import java.math.BigInteger

fun randInt(s: Int, n: Int): MutableList<Int> {
    var next: BigInteger = (Math.floorMod(((s * 1664525) + 1013904223), 2147483647)).toBigInteger()
    return mutableListOf<Int>(next.toInt(), (next.remainder(n.toBigInteger())).toInt())
}

fun padLeft(s: String, w: Int): String {
    var res: String = ""
    var n: BigInteger = (w - s.length).toBigInteger()
    while (n.compareTo(0.toBigInteger()) > 0) {
        res = res + " "
        n = n.subtract(1.toBigInteger())
    }
    return res + s
}

fun makeSeq(s: Int, le: Int): MutableList<Any?> {
    var s: Int = s
    var bases: String = "ACGT"
    var out: String = ""
    var i: Int = 0
    while (i < le) {
        var r: MutableList<Int> = randInt(s, 4)
        s = r[0]!!
        var idx: Int = (r[1]!!).toInt()
        out = out + (bases.substring(idx, idx + 1)).toString()
        i = i + 1
    }
    return mutableListOf<Any?>(s as Any?, out as Any?)
}

fun mutate(s: Int, dna: String, w: MutableList<Int>): MutableList<Any?> {
    var s: Int = s
    var bases: String = "ACGT"
    var le: Int = dna.length
    var r: MutableList<Int> = randInt(s, le)
    s = r[0]!!
    var p: Int = (r[1]!!).toInt()
    r = randInt(s, 300)
    s = r[0]!!
    var x: Int = (r[1]!!).toInt()
    var arr: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < le) {
        arr = run { val _tmp = arr.toMutableList(); _tmp.add(dna.substring(i, i + 1) as String); _tmp } as MutableList<String>
        i = i + 1
    }
    if (x < w[0]!!) {
        r = randInt(s, 4)
        s = r[0]!!
        var idx: Int = (r[1]!!).toInt()
        var b: String = bases.substring(idx, idx + 1)
        println(((((("  Change @" + padLeft(p.toString(), 3)) + " '") + arr[p]!!) + "' to '") + b) + "'")
        arr[p] = b
    } else {
        if (x < (w[0]!! + w[1]!!)) {
            println(((("  Delete @" + padLeft(p.toString(), 3)) + " '") + arr[p]!!) + "'")
            var j: Int = p
            while (j < (arr.size - 1)) {
                arr[j] = arr[j + 1]!!
                j = j + 1
            }
            arr = arr.subList(0, arr.size - 1)
        } else {
            r = randInt(s, 4)
            s = r[0]!!
            var idx2: Int = (r[1]!!).toInt()
            var b: String = bases.substring(idx2, idx2 + 1)
            arr = run { val _tmp = arr.toMutableList(); _tmp.add(""); _tmp } as MutableList<String>
            var j: BigInteger = (arr.size - 1).toBigInteger()
            while (j.compareTo(p.toBigInteger()) > 0) {
                arr[(j).toInt()] = arr[(j.subtract(1.toBigInteger())).toInt()]!!
                j = j.subtract(1.toBigInteger())
            }
            println(((("  Insert @" + padLeft(p.toString(), 3)) + " '") + b) + "'")
            arr[p] = b
        }
    }
    var out: String = ""
    i = 0
    while (i < arr.size) {
        out = out + arr[i]!!
        i = i + 1
    }
    return mutableListOf<Any?>(s as Any?, out as Any?)
}

fun prettyPrint(dna: String, rowLen: Int): Unit {
    println("SEQUENCE:")
    var le: Int = dna.length
    var i: Int = 0
    while (i < le) {
        var k: BigInteger = (i + rowLen).toBigInteger()
        if (k.compareTo(le.toBigInteger()) > 0) {
            k = le.toBigInteger()
        }
        println((padLeft(i.toString(), 5) + ": ") + dna.substring(i, (k).toInt()))
        i = i + rowLen
    }
    var a: Int = 0
    var c: Int = 0
    var g: Int = 0
    var t: Int = 0
    var idx: Int = 0
    while (idx < le) {
        var ch: String = dna.substring(idx, idx + 1)
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
        idx = idx + 1
    }
    println("")
    println("BASE COUNT:")
    println("    A: " + padLeft(a.toString(), 3))
    println("    C: " + padLeft(c.toString(), 3))
    println("    G: " + padLeft(g.toString(), 3))
    println("    T: " + padLeft(t.toString(), 3))
    println("    ------")
    println("    Σ: " + le.toString())
    println("    ======")
}

fun wstring(w: MutableList<Int>): String {
    return ((((("  Change: " + (w[0]!!).toString()) + "\n  Delete: ") + (w[1]!!).toString()) + "\n  Insert: ") + (w[2]!!).toString()) + "\n"
}

fun user_main(): Unit {
    var seed: Int = 1
    var res: MutableList<Any?> = makeSeq(seed, 250)
    seed = (res[0] as Any?) as Int
    var dna: String = res[1] as String
    prettyPrint(dna, 50)
    var muts: Int = 10
    var w: MutableList<Int> = mutableListOf(100, 100, 100)
    println("\nWEIGHTS (ex 300):")
    println(wstring(w))
    println(("MUTATIONS (" + muts.toString()) + "):")
    var i: Int = 0
    while (i < muts) {
        res = mutate(seed, dna, w)
        seed = (res[0] as Any?) as Int
        dna = res[1] as String
        i = i + 1
    }
    println("")
    prettyPrint(dna, 50)
}

fun main() {
    user_main()
}
