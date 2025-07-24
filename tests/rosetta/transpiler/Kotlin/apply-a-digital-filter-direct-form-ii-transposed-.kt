val a: MutableList<Double> = mutableListOf(1.0, 0.0 - 0.00000000000000027756, 0.33333333, 0.0 - 0.0000000000000000185)
val b: MutableList<Double> = mutableListOf(0.16666667, 0.5, 0.5, 0.16666667)
val sig: MutableList<Double> = mutableListOf(0.0 - 0.917843918645, 0.141984778794, 1.20536903482, 0.190286794412, 0.0 - 0.662370894973, 0.0 - 1.00700480494, 0.0 - 0.404707073677, 0.800482325044, 0.743500089861, 1.01090520172, 0.741527555207, 0.277841675195, 0.400833448236, 0.0 - 0.2085993586, 0.0 - 0.172842103641, 0.0 - 0.134316096293, 0.0259303398477, 0.490105989562, 0.549391221511, 0.9047198589)
val res: MutableList<Double> = applyFilter(sig, a, b)
var k: Int = 0
fun applyFilter(input: MutableList<Double>, a: MutableList<Double>, b: MutableList<Double>): MutableList<Double> {
    var out: MutableList<Double> = mutableListOf()
    val scale: Double = 1.0 / a[0]
    var i: Int = 0
    while (i < input.size) {
        var tmp: Double = 0.0
        var j: Int = 0
        while ((j <= i) && (j < b.size)) {
            tmp = tmp + (b[j] * input[i - j])
            j = j + 1
        }
        j = 0
        while ((j < i) && ((j + 1) < a.size)) {
            tmp = tmp - (a[j + 1] * out[(i - j) - 1])
            j = j + 1
        }
        out = run { val _tmp = out.toMutableList(); _tmp.add(tmp * scale); _tmp } as MutableList<Double>
        i = i + 1
    }
    return out
}

fun main() {
    while (k < res.size) {
        println(res[k])
        k = k + 1
    }
}
