import java.math.BigInteger

fun int_to_float(x: Int): Double {
    return x * 1.0
}

fun floor_int(x: Double): Int {
    var i: Int = (0).toInt()
    while (int_to_float(i + 1) <= x) {
        i = i + 1
    }
    return i
}

fun set_at_float(xs: MutableList<Double>, idx: Int, value: Double): MutableList<Double> {
    var i: Int = (0).toInt()
    var res: MutableList<Double> = mutableListOf<Double>()
    while (i < xs.size) {
        if (i == idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun set_at_list_float(xs: MutableList<MutableList<Double>>, idx: Int, value: MutableList<Double>): MutableList<MutableList<Double>> {
    var i: Int = (0).toInt()
    var res: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    while (i < xs.size) {
        if (i == idx) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(value); _tmp }
        } else {
            res = run { val _tmp = res.toMutableList(); _tmp.add(xs[i]!!); _tmp }
        }
        i = i + 1
    }
    return res
}

fun sort_float(xs: MutableList<Double>): MutableList<Double> {
    var res: MutableList<Double> = xs
    var i: Int = (1).toInt()
    while (i < res.size) {
        var key: Double = res[i]!!
        var j: BigInteger = ((i - 1).toBigInteger())
        while ((j.compareTo((0).toBigInteger()) >= 0) && (res[(j).toInt()]!! > key)) {
            res = set_at_float(res, ((j.add((1).toBigInteger())).toInt()), res[(j).toInt()]!!)
            j = j.subtract((1).toBigInteger())
        }
        res = set_at_float(res, ((j.add((1).toBigInteger())).toInt()), key)
        i = i + 1
    }
    return res
}

fun bucket_sort_with_count(xs: MutableList<Double>, bucket_count: Int): MutableList<Double> {
    if ((xs.size == 0) || (bucket_count <= 0)) {
        return mutableListOf<Double>()
    }
    var min_value: Double = xs[0]!!
    var max_value: Double = xs[0]!!
    var i: Int = (1).toInt()
    while (i < xs.size) {
        if (xs[i]!! < min_value) {
            min_value = xs[i]!!
        }
        if (xs[i]!! > max_value) {
            max_value = xs[i]!!
        }
        i = i + 1
    }
    if (max_value == min_value) {
        return xs
    }
    var bucket_size: Double = (max_value - min_value) / int_to_float(bucket_count)
    var buckets: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    i = 0
    while (i < bucket_count) {
        buckets = run { val _tmp = buckets.toMutableList(); _tmp.add(mutableListOf<Double>()); _tmp }
        i = i + 1
    }
    i = 0
    while (i < xs.size) {
        var _val: Double = xs[i]!!
        var idx: Int = (floor_int((_val - min_value) / bucket_size)).toInt()
        if (idx < 0) {
            idx = 0
        }
        if (idx >= bucket_count) {
            idx = bucket_count - 1
        }
        var bucket: MutableList<Double> = buckets[idx]!!
        bucket = run { val _tmp = bucket.toMutableList(); _tmp.add(_val); _tmp }
        buckets = set_at_list_float(buckets, idx, bucket)
        i = i + 1
    }
    var result: MutableList<Double> = mutableListOf<Double>()
    i = 0
    while (i < buckets.size) {
        var sorted_bucket: MutableList<Double> = sort_float(buckets[i]!!)
        var j: Int = (0).toInt()
        while (j < sorted_bucket.size) {
            result = run { val _tmp = result.toMutableList(); _tmp.add(sorted_bucket[j]!!); _tmp }
            j = j + 1
        }
        i = i + 1
    }
    return result
}

fun bucket_sort(xs: MutableList<Double>): MutableList<Double> {
    return bucket_sort_with_count(xs, 10)
}

fun main() {
    println(bucket_sort(mutableListOf(0.0 - 1.0, 2.0, 0.0 - 5.0, 0.0)).toString())
    println(bucket_sort(mutableListOf(9.0, 8.0, 7.0, 6.0, 0.0 - 12.0)).toString())
    println(bucket_sort(mutableListOf(0.4, 1.2, 0.1, 0.2, 0.0 - 0.9)).toString())
    println(bucket_sort(mutableListOf<Double>()).toString())
    println(bucket_sort(mutableListOf(0.0 - 10000000000.0, 10000000000.0)).toString())
}
