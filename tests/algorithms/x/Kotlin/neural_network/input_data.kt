import java.math.BigInteger

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

fun <T> concat(a: MutableList<T>, b: MutableList<T>): MutableList<T> {
    val res = mutableListOf<T>()
    res.addAll(a)
    res.addAll(b)
    return res
}

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

data class DataSet(var images: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>(), var labels: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>(), var num_examples: Int = 0, var index_in_epoch: Int = 0, var epochs_completed: Int = 0)
data class Datasets(var train: DataSet = DataSet(images = mutableListOf<MutableList<Int>>(), labels = mutableListOf<MutableList<Int>>(), num_examples = 0, index_in_epoch = 0, epochs_completed = 0), var validation: DataSet = DataSet(images = mutableListOf<MutableList<Int>>(), labels = mutableListOf<MutableList<Int>>(), num_examples = 0, index_in_epoch = 0, epochs_completed = 0), var test_ds: DataSet = DataSet(images = mutableListOf<MutableList<Int>>(), labels = mutableListOf<MutableList<Int>>(), num_examples = 0, index_in_epoch = 0, epochs_completed = 0))
data class BatchResult(var dataset: DataSet = DataSet(images = mutableListOf<MutableList<Int>>(), labels = mutableListOf<MutableList<Int>>(), num_examples = 0, index_in_epoch = 0, epochs_completed = 0), var images: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>(), var labels: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>())
fun dense_to_one_hot(labels: MutableList<Int>, num_classes: Int): MutableList<MutableList<Int>> {
    var result: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var i: Int = (0).toInt()
    while (i < labels.size) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var j: Int = (0).toInt()
        while (j < num_classes) {
            if (j == labels[i]!!) {
                row = run { val _tmp = row.toMutableList(); _tmp.add(1); _tmp }
            } else {
                row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            }
            j = j + 1
        }
        result = run { val _tmp = result.toMutableList(); _tmp.add(row); _tmp }
        i = i + 1
    }
    return result
}

fun new_dataset(images: MutableList<MutableList<Int>>, labels: MutableList<MutableList<Int>>): DataSet {
    return DataSet(images = images, labels = labels, num_examples = images.size, index_in_epoch = 0, epochs_completed = 0)
}

fun next_batch(ds: DataSet, batch_size: Int): BatchResult {
    var start: Int = (ds.index_in_epoch).toInt()
    if ((start + batch_size) > ds.num_examples) {
        var rest: Int = (ds.num_examples - start).toInt()
        var images_rest: MutableList<MutableList<Int>> = _sliceList(ds.images, start, ds.num_examples)
        var labels_rest: MutableList<MutableList<Int>> = _sliceList(ds.labels, start, ds.num_examples)
        var new_index: Int = (batch_size - rest).toInt()
        var images_new: MutableList<MutableList<Int>> = _sliceList(ds.images, 0, new_index)
        var labels_new: MutableList<MutableList<Int>> = _sliceList(ds.labels, 0, new_index)
        var batch_images = concat(images_rest, images_new)
        var batch_labels = concat(labels_rest, labels_new)
        var new_ds: DataSet = DataSet(images = ds.images, labels = ds.labels, num_examples = ds.num_examples, index_in_epoch = new_index, epochs_completed = ds.epochs_completed + 1)
        return BatchResult(dataset = new_ds, images = batch_images, labels = batch_labels)
    } else {
        var end: Int = (start + batch_size).toInt()
        var batch_images = _sliceList(ds.images, start, end)
        var batch_labels = _sliceList(ds.labels, start, end)
        var new_ds: DataSet = DataSet(images = ds.images, labels = ds.labels, num_examples = ds.num_examples, index_in_epoch = end, epochs_completed = ds.epochs_completed)
        return BatchResult(dataset = new_ds, images = batch_images, labels = batch_labels)
    }
}

fun read_data_sets(train_images: MutableList<MutableList<Int>>, train_labels_raw: MutableList<Int>, test_images: MutableList<MutableList<Int>>, test_labels_raw: MutableList<Int>, validation_size: Int, num_classes: Int): Datasets {
    var train_labels: MutableList<MutableList<Int>> = dense_to_one_hot(train_labels_raw, num_classes)
    var test_labels: MutableList<MutableList<Int>> = dense_to_one_hot(test_labels_raw, num_classes)
    var validation_images: MutableList<MutableList<Int>> = _sliceList(train_images, 0, validation_size)
    var validation_labels: MutableList<MutableList<Int>> = _sliceList(train_labels, 0, validation_size)
    var train_images_rest: MutableList<MutableList<Int>> = _sliceList(train_images, validation_size, train_images.size)
    var train_labels_rest: MutableList<MutableList<Int>> = _sliceList(train_labels, validation_size, train_labels.size)
    var train: DataSet = new_dataset(train_images_rest, train_labels_rest)
    var validation: DataSet = new_dataset(validation_images, validation_labels)
    var testset: DataSet = new_dataset(test_images, test_labels)
    return Datasets(train = train, validation = validation, test_ds = testset)
}

fun user_main(): Unit {
    var train_images: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, 1), mutableListOf(1, 2), mutableListOf(2, 3), mutableListOf(3, 4), mutableListOf(4, 5))
    var train_labels_raw: MutableList<Int> = mutableListOf(0, 1, 2, 3, 4)
    var test_images: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(5, 6), mutableListOf(6, 7))
    var test_labels_raw: MutableList<Int> = mutableListOf(5, 6)
    var data: Datasets = read_data_sets(train_images, train_labels_raw, test_images, test_labels_raw, 2, 10)
    var ds: DataSet = data.train
    var res: BatchResult = next_batch(ds, 2)
    ds = res.dataset
    println(res.images.toString())
    println(res.labels.toString())
    res = next_batch(ds, 2)
    ds = res.dataset
    println(res.images.toString())
    println(res.labels.toString())
    res = next_batch(ds, 2)
    ds = res.dataset
    println(res.images.toString())
    println(res.labels.toString())
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
