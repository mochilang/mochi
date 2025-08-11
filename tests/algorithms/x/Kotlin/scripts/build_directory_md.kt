import java.math.BigInteger

fun _sliceStr(s: String, start: Int, end: Int): String {
    val st = if (start < 0) 0 else start
    val en = if (end > s.length) s.length else end
    return if (st >= en) "" else s.substring(st, en)
}

fun <T> _listSet(lst: MutableList<T>, idx: Int, v: T) { while (lst.size <= idx) lst.add(v); lst[idx] = v }

fun <T> _sliceList(lst: MutableList<T>, start: Int, end: Int): MutableList<T> {
    val st = if (start < 0) 0 else start
    val en = if (end > lst.size) lst.size else end
    if (st >= en) return mutableListOf()
    return lst.subList(st, en).toMutableList()
}

var sample: MutableList<String> = mutableListOf("data_structures/linked_list.py", "data_structures/binary_tree.py", "math/number_theory/prime_check.py", "math/number_theory/greatest_common_divisor.ipynb")
fun split(s: String, sep: String): MutableList<String> {
    var parts: MutableList<String> = mutableListOf<String>()
    var cur: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        if ((((sep.length > 0) && ((i + sep.length) <= s.length) as Boolean)) && (s.substring(i, i + sep.length) == sep)) {
            parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
            cur = ""
            i = i + sep.length
        } else {
            cur = cur + s.substring(i, i + 1)
            i = i + 1
        }
    }
    parts = run { val _tmp = parts.toMutableList(); _tmp.add(cur); _tmp }
    return parts
}

fun join(xs: MutableList<String>, sep: String): String {
    var res: String = ""
    var i: Int = (0).toInt()
    while (i < xs.size) {
        if (i > 0) {
            res = res + sep
        }
        res = res + xs[i]!!
        i = i + 1
    }
    return res
}

fun repeat(s: String, n: Int): String {
    var out: String = ""
    var i: Int = (0).toInt()
    while (i < n) {
        out = out + s
        i = i + 1
    }
    return out
}

fun replace_char(s: String, old: String, new: String): String {
    var out: String = ""
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        if (c == old) {
            out = out + new
        } else {
            out = out + c
        }
        i = i + 1
    }
    return out
}

fun contains(s: String, sub: String): Boolean {
    if (sub.length == 0) {
        return true
    }
    var i: Int = (0).toInt()
    while ((i + sub.length) <= s.length) {
        if (s.substring(i, i + sub.length) == sub) {
            return true
        }
        i = i + 1
    }
    return false
}

fun file_extension(name: String): String {
    var i: BigInteger = ((name.length - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        if (name.substring((i).toInt(), (i.add((1).toBigInteger())).toInt()) == ".") {
            return _sliceStr(name, (i).toInt(), name.length)
        }
        i = i.subtract((1).toBigInteger())
    }
    return ""
}

fun remove_extension(name: String): String {
    var i: BigInteger = ((name.length - 1).toBigInteger())
    while (i.compareTo((0).toBigInteger()) >= 0) {
        if (name.substring((i).toInt(), (i.add((1).toBigInteger())).toInt()) == ".") {
            return _sliceStr(name, 0, (i).toInt())
        }
        i = i.subtract((1).toBigInteger())
    }
    return name
}

fun title_case(s: String): String {
    var out: String = ""
    var cap: Boolean = true
    var i: Int = (0).toInt()
    while (i < s.length) {
        var c: String = s.substring(i, i + 1)
        if (c == " ") {
            out = out + c
            cap = true
        } else {
            if ((cap as Boolean)) {
                out = out + (c.toUpperCase()).toString()
                cap = false
            } else {
                out = out + (c.toLowerCase()).toString()
            }
        }
        i = i + 1
    }
    return out
}

fun count_char(s: String, ch: String): Int {
    var cnt: Int = (0).toInt()
    var i: Int = (0).toInt()
    while (i < s.length) {
        if (s.substring(i, i + 1) == ch) {
            cnt = cnt + 1
        }
        i = i + 1
    }
    return cnt
}

fun md_prefix(level: Int): String {
    if (level == 0) {
        return "\n##"
    }
    return repeat("  ", level) + "*"
}

fun print_path(old_path: String, new_path: String): String {
    var old_parts: MutableList<String> = split(old_path, "/")
    var new_parts: MutableList<String> = split(new_path, "/")
    var i: Int = (0).toInt()
    while (i < new_parts.size) {
        if ((((i >= old_parts.size) || (old_parts[i]!! != new_parts[i]!!) as Boolean)) && (new_parts[i]!! != "")) {
            var title: String = title_case(replace_char(new_parts[i]!!, "_", " "))
            println((md_prefix(i) + " ") + title)
        }
        i = i + 1
    }
    return new_path
}

fun sort_strings(xs: MutableList<String>): MutableList<String> {
    var arr: MutableList<String> = xs
    var i: Int = (0).toInt()
    while (i < arr.size) {
        var min_idx: Int = (i).toInt()
        var j: BigInteger = ((i + 1).toBigInteger())
        while (j.compareTo((arr.size).toBigInteger()) < 0) {
            if (arr[(j).toInt()]!! < arr[min_idx]!!) {
                min_idx = (j.toInt())
            }
            j = j.add((1).toBigInteger())
        }
        var tmp: String = arr[i]!!
        _listSet(arr, i, arr[min_idx]!!)
        _listSet(arr, min_idx, tmp)
        i = i + 1
    }
    return arr
}

fun good_file_paths(paths: MutableList<String>): MutableList<String> {
    var res: MutableList<String> = mutableListOf<String>()
    for (p in paths) {
        var parts: MutableList<String> = split(p, "/")
        var skip: Boolean = false
        var k: Int = (0).toInt()
        while (k < (parts.size - 1)) {
            var part: String = parts[k]!!
            if ((((((part == "scripts") || (_sliceStr(part, 0, 1) == ".") as Boolean)) || (_sliceStr(part, 0, 1) == "_") as Boolean)) || (part.contains("venv") as Boolean)) {
                skip = true
            }
            k = k + 1
        }
        if ((skip as Boolean)) {
            continue
        }
        var filename: String = parts[parts.size - 1]!!
        if (filename == "__init__.py") {
            continue
        }
        var ext: String = file_extension(filename)
        if ((ext == ".py") || (ext == ".ipynb")) {
            res = run { val _tmp = res.toMutableList(); _tmp.add(p); _tmp }
        }
    }
    return res
}

fun print_directory_md(paths: MutableList<String>): Unit {
    var files: MutableList<String> = sort_strings(good_file_paths(paths))
    var old_path: String = ""
    var i: Int = (0).toInt()
    while (i < files.size) {
        var fp: String = files[i]!!
        var parts: MutableList<String> = split(fp, "/")
        var filename: String = parts[parts.size - 1]!!
        var filepath: String = ""
        if (parts.size > 1) {
            filepath = join(_sliceList(parts, 0, parts.size - 1), "/")
        }
        if (filepath != old_path) {
            old_path = print_path(old_path, filepath)
        }
        var indent: Int = (0).toInt()
        if (filepath.length > 0) {
            indent = count_char(filepath, "/") + 1
        }
        var url: String = replace_char(fp, " ", "%20")
        var name: String = title_case(replace_char(remove_extension(filename), "_", " "))
        println(((((md_prefix(indent) + " [") + name) + "](") + url) + ")")
        i = i + 1
    }
}

fun main() {
    print_directory_md(sample)
}
