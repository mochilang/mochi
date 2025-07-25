// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
fun <T> append(list: MutableList<T>, item: T): MutableList<T> {
    val res = list.toMutableList()
    res.add(item)
    return res
}
// Code generated from append-a-record-to-the-end-of-a-text-file.mochi

/**
 * Auto-generated from Mochi
 * @return MutableList<String>
 */
fun writeTwo(): MutableList<String> {
    return mutableListOf("jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash", "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash")
}

/**
 * Auto-generated from Mochi
 * @param lines MutableList<String>
 * @return MutableList<String>
 */
fun appendOneMore(lines: MutableList<String>): MutableList<String> {
    return append(lines, "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash")
}

/**
 * Auto-generated from Mochi
 */
fun main(): Unit {
    var lines = writeTwo()
    lines = appendOneMore(lines)
    if (lines.size >= 3 && lines[2] == "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash") {
        println("append okay")
    }
    else {
        println("it didn't work")
    }
}

