fun mapString(s: String, f: (String) -> String): String {
    var out: String = ""
    var i: Int = 0
    while (i < s.length) {
        out = out + (f(s.substring(i, i + 1))).toString()
        i = i + 1
    }
    return out
}

fun user_main(): Unit {
    var fn: (String) -> String = { r: String -> if (r == " ") "" else r }
    mapString("Spaces removed", fn)
    mapString("Test", ({ r: String -> r.toLowerCase() } as (String) -> String))
    mapString("shift", ({ r: String -> r } as (String) -> String))
}

fun main() {
    user_main()
}
