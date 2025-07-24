fun writeTwo(): MutableList<String> {
    return mutableListOf("jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash", "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash")
}

fun appendOneMore(lines: MutableList<String>): MutableList<String> {
    return run { val _tmp = lines.toMutableList(); _tmp.add("xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"); _tmp } as MutableList<String>
}

fun user_main(): Unit {
    var lines: MutableList<String> = writeTwo()
    lines = appendOneMore(lines)
    if ((lines.size >= 3) && (lines[2] == "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash")) {
        println("append okay")
    } else {
        println("it didn't work")
    }
}

fun main() {
    user_main()
}
