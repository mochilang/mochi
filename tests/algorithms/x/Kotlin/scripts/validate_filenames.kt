fun indexOf(s: String, sub: String): Int {
    var n: Int = (s.length).toInt()
    var m: Int = (sub.length).toInt()
    var i: Int = (0).toInt()
    while (i <= (n - m)) {
        if (s.substring(i, i + m) == sub) {
            return i
        }
        i = i + 1
    }
    return 0 - 1
}

fun contains(s: String, sub: String): Boolean {
    return s.indexOf(sub) >= 0
}

fun validate(files: MutableList<String>): Int {
    var upper: MutableList<String> = mutableListOf<String>()
    var space: MutableList<String> = mutableListOf<String>()
    var hyphen: MutableList<String> = mutableListOf<String>()
    var nodir: MutableList<String> = mutableListOf<String>()
    for (f in files) {
        if (f != f.toLowerCase()) {
            upper = run { val _tmp = upper.toMutableList(); _tmp.add(f); _tmp }
        }
        if (f.contains(" ")) {
            space = run { val _tmp = space.toMutableList(); _tmp.add(f); _tmp }
        }
        if ((f.contains("-") as Boolean) && (f.contains("/site-packages/") == false)) {
            hyphen = run { val _tmp = hyphen.toMutableList(); _tmp.add(f); _tmp }
        }
        if (!((f.contains("/")) as Boolean)) {
            nodir = run { val _tmp = nodir.toMutableList(); _tmp.add(f); _tmp }
        }
    }
    if (upper.size > 0) {
        println(upper.size.toString() + " files contain uppercase characters:")
        for (f in upper) {
            println(f)
        }
        println("")
    }
    if (space.size > 0) {
        println(space.size.toString() + " files contain space characters:")
        for (f in space) {
            println(f)
        }
        println("")
    }
    if (hyphen.size > 0) {
        println(hyphen.size.toString() + " files contain hyphen characters:")
        for (f in hyphen) {
            println(f)
        }
        println("")
    }
    if (nodir.size > 0) {
        println(nodir.size.toString() + " files are not in a directory:")
        for (f in nodir) {
            println(f)
        }
        println("")
    }
    return ((upper.size + space.size) + hyphen.size) + nodir.size
}

fun user_main(): Unit {
    var files: MutableList<String> = mutableListOf("scripts/Validate_filenames.py", "good/file.txt", "bad file.txt", "/site-packages/pkg-name.py", "nopath", "src/hyphen-name.py")
    var bad: Int = (validate(files)).toInt()
    println(bad.toString())
}

fun main() {
    user_main()
}
