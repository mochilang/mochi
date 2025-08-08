fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

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

data class MosaicResult(var img: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>(), var annos: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>(), var path: String = "")
fun update_image_and_anno(all_img_list: MutableList<String>, all_annos: MutableList<MutableList<MutableList<Double>>>, idxs: MutableList<Int>, output_size: MutableList<Int>, scale_range: MutableList<Double>, filter_scale: Double): MosaicResult {
    var height: Int = output_size[0]!!
    var width: Int = output_size[1]!!
    var output_img: MutableList<MutableList<Int>> = mutableListOf<MutableList<Int>>()
    var r: Int = 0
    while (r < height) {
        var row: MutableList<Int> = mutableListOf<Int>()
        var c: Int = 0
        while (c < width) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp }
            c = c + 1
        }
        output_img = run { val _tmp = output_img.toMutableList(); _tmp.add(row); _tmp }
        r = r + 1
    }
    var scale_x: Double = (scale_range[0]!! + scale_range[1]!!) / 2.0
    var scale_y: Double = (scale_range[0]!! + scale_range[1]!!) / 2.0
    var divid_point_x: Int = ((scale_x * ((width.toDouble()))).toInt())
    var divid_point_y: Int = ((scale_y * ((height.toDouble()))).toInt())
    var new_anno: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
    var path_list: MutableList<String> = mutableListOf<String>()
    var i: Int = 0
    while (i < idxs.size) {
        var index: Int = idxs[i]!!
        var path: String = all_img_list[index]!!
        path_list = run { val _tmp = path_list.toMutableList(); _tmp.add(path); _tmp }
        var img_annos: MutableList<MutableList<Double>> = all_annos[index]!!
        if (i == 0) {
            var y0: Int = 0
            while (y0 < divid_point_y) {
                var x0: Int = 0
                while (x0 < divid_point_x) {
                    _listSet(output_img[y0]!!, x0, i + 1)
                    x0 = x0 + 1
                }
                y0 = y0 + 1
            }
            var j0: Int = 0
            while (j0 < img_annos.size) {
                var bbox: MutableList<Double> = img_annos[j0]!!
                var xmin: Double = bbox[1]!! * scale_x
                var ymin: Double = bbox[2]!! * scale_y
                var xmax: Double = bbox[3]!! * scale_x
                var ymax: Double = bbox[4]!! * scale_y
                new_anno = run { val _tmp = new_anno.toMutableList(); _tmp.add(mutableListOf(bbox[0]!!, xmin, ymin, xmax, ymax)); _tmp }
                j0 = j0 + 1
            }
        } else {
            if (i == 1) {
                var y1: Int = 0
                while (y1 < divid_point_y) {
                    var x1: Int = divid_point_x
                    while (x1 < width) {
                        _listSet(output_img[y1]!!, x1, i + 1)
                        x1 = x1 + 1
                    }
                    y1 = y1 + 1
                }
                var j1: Int = 0
                while (j1 < img_annos.size) {
                    var bbox1: MutableList<Double> = img_annos[j1]!!
                    var xmin1: Double = scale_x + (bbox1[1]!! * (1.0 - scale_x))
                    var ymin1: Double = bbox1[2]!! * scale_y
                    var xmax1: Double = scale_x + (bbox1[3]!! * (1.0 - scale_x))
                    var ymax1: Double = bbox1[4]!! * scale_y
                    new_anno = run { val _tmp = new_anno.toMutableList(); _tmp.add(mutableListOf(bbox1[0]!!, xmin1, ymin1, xmax1, ymax1)); _tmp }
                    j1 = j1 + 1
                }
            } else {
                if (i == 2) {
                    var y2: Int = divid_point_y
                    while (y2 < height) {
                        var x2: Int = 0
                        while (x2 < divid_point_x) {
                            _listSet(output_img[y2]!!, x2, i + 1)
                            x2 = x2 + 1
                        }
                        y2 = y2 + 1
                    }
                    var j2: Int = 0
                    while (j2 < img_annos.size) {
                        var bbox2: MutableList<Double> = img_annos[j2]!!
                        var xmin2: Double = bbox2[1]!! * scale_x
                        var ymin2: Double = scale_y + (bbox2[2]!! * (1.0 - scale_y))
                        var xmax2: Double = bbox2[3]!! * scale_x
                        var ymax2: Double = scale_y + (bbox2[4]!! * (1.0 - scale_y))
                        new_anno = run { val _tmp = new_anno.toMutableList(); _tmp.add(mutableListOf(bbox2[0]!!, xmin2, ymin2, xmax2, ymax2)); _tmp }
                        j2 = j2 + 1
                    }
                } else {
                    var y3: Int = divid_point_y
                    while (y3 < height) {
                        var x3: Int = divid_point_x
                        while (x3 < width) {
                            _listSet(output_img[y3]!!, x3, i + 1)
                            x3 = x3 + 1
                        }
                        y3 = y3 + 1
                    }
                    var j3: Int = 0
                    while (j3 < img_annos.size) {
                        var bbox3: MutableList<Double> = img_annos[j3]!!
                        var xmin3: Double = scale_x + (bbox3[1]!! * (1.0 - scale_x))
                        var ymin3: Double = scale_y + (bbox3[2]!! * (1.0 - scale_y))
                        var xmax3: Double = scale_x + (bbox3[3]!! * (1.0 - scale_x))
                        var ymax3: Double = scale_y + (bbox3[4]!! * (1.0 - scale_y))
                        new_anno = run { val _tmp = new_anno.toMutableList(); _tmp.add(mutableListOf(bbox3[0]!!, xmin3, ymin3, xmax3, ymax3)); _tmp }
                        j3 = j3 + 1
                    }
                }
            }
        }
        i = i + 1
    }
    if (filter_scale > 0.0) {
        var filtered: MutableList<MutableList<Double>> = mutableListOf<MutableList<Double>>()
        var k: Int = 0
        while (k < new_anno.size) {
            var anno: MutableList<Double> = new_anno[k]!!
            var w: Double = anno[3]!! - anno[1]!!
            var h: Double = anno[4]!! - anno[2]!!
            if ((filter_scale < w) && (filter_scale < h)) {
                filtered = run { val _tmp = filtered.toMutableList(); _tmp.add(anno); _tmp }
            }
            k = k + 1
        }
        new_anno = filtered
    }
    return MosaicResult(img = output_img, annos = new_anno, path = path_list[0]!!)
}

fun user_main(): Unit {
    var all_img_list: MutableList<String> = mutableListOf("img0.jpg", "img1.jpg", "img2.jpg", "img3.jpg")
    var all_annos: MutableList<MutableList<MutableList<Double>>> = mutableListOf(mutableListOf(mutableListOf(0.0, 0.1, 0.1, 0.4, 0.4)), mutableListOf(mutableListOf(1.0, 0.2, 0.3, 0.5, 0.7)), mutableListOf(mutableListOf(2.0, 0.6, 0.2, 0.9, 0.5)), mutableListOf(mutableListOf(3.0, 0.5, 0.5, 0.8, 0.8)))
    var idxs: MutableList<Int> = mutableListOf(0, 1, 2, 3)
    var output_size: MutableList<Int> = mutableListOf(100, 100)
    var scale_range: MutableList<Double> = mutableListOf(0.4, 0.6)
    var filter_scale: Double = 0.05
    var res: MosaicResult = update_image_and_anno(all_img_list, all_annos, idxs, output_size, scale_range, filter_scale)
    var new_annos: MutableList<MutableList<Double>> = res.annos
    var path: String = res.path
    println("Base image: " + path)
    println("Mosaic annotation count: " + new_annos.size.toString())
    var i: Int = 0
    while (i < new_annos.size) {
        var a: MutableList<Double> = new_annos[i]!!
        println(((((((((a[0]!!).toString() + " ") + (a[1]!!).toString()) + " ") + (a[2]!!).toString()) + " ") + (a[3]!!).toString()) + " ") + (a[4]!!).toString())
        i = i + 1
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
