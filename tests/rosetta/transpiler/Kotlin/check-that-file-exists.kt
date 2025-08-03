fun printStat(fs: MutableMap<String, Boolean>, path: String): Unit {
    if (path in fs) {
        if ((((fs)[path] as Boolean) as Boolean)) {
            println(path + " is a directory")
        } else {
            println(path + " is a file")
        }
    } else {
        println(("stat " + path) + ": no such file or directory")
    }
}

fun user_main(): Unit {
    var fs: MutableMap<String, Boolean> = mutableMapOf<String, Boolean>()
    (fs)["docs"] = true
    for (p in mutableListOf("input.txt", "/input.txt", "docs", "/docs")) {
        printStat(fs, p)
    }
}

fun main() {
    user_main()
}
