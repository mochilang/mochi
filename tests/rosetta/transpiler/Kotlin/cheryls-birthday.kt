data class Birthday(var month: Int = 0, var day: Int = 0)
var choices: MutableList<Birthday> = mutableListOf(Birthday(month = 5, day = 15), Birthday(month = 5, day = 16), Birthday(month = 5, day = 19), Birthday(month = 6, day = 17), Birthday(month = 6, day = 18), Birthday(month = 7, day = 14), Birthday(month = 7, day = 16), Birthday(month = 8, day = 14), Birthday(month = 8, day = 15), Birthday(month = 8, day = 17))
var filtered: MutableList<Birthday> = mutableListOf<Birthday>()
var filtered2: MutableList<Birthday> = mutableListOf<Birthday>()
var filtered3: MutableList<Birthday> = mutableListOf<Birthday>()
var filtered4: MutableList<Birthday> = mutableListOf<Birthday>()
fun monthUnique(b: Birthday, list: MutableList<Birthday>): Boolean {
    var c: Int = 0
    for (x in list) {
        if (x.month == b.month) {
            c = c + 1
        }
    }
    return c == 1
}

fun dayUnique(b: Birthday, list: MutableList<Birthday>): Boolean {
    var c: Int = 0
    for (x in list) {
        if (x.day == b.day) {
            c = c + 1
        }
    }
    return c == 1
}

fun monthWithUniqueDay(b: Birthday, list: MutableList<Birthday>): Boolean {
    for (x in list) {
        if ((x.month == b.month) && dayUnique(x, list)) {
            return true
        }
    }
    return false
}

fun bstr(b: Birthday): String {
    var months: MutableList<String> = mutableListOf("", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    return (months[b.month]!! + " ") + b.day.toString()
}

fun main() {
    for (bd in choices) {
        if (!monthUnique(bd, choices)) {
            filtered = run { val _tmp = filtered.toMutableList(); _tmp.add(bd); _tmp }
        }
    }
    for (bd in filtered) {
        if (!monthWithUniqueDay(bd, filtered)) {
            filtered2 = run { val _tmp = filtered2.toMutableList(); _tmp.add(bd); _tmp }
        }
    }
    for (bd in filtered2) {
        if (((dayUnique(bd, filtered2)) as Boolean)) {
            filtered3 = run { val _tmp = filtered3.toMutableList(); _tmp.add(bd); _tmp }
        }
    }
    for (bd in filtered3) {
        if (((monthUnique(bd, filtered3)) as Boolean)) {
            filtered4 = run { val _tmp = filtered4.toMutableList(); _tmp.add(bd); _tmp }
        }
    }
    if (filtered4.size == 1) {
        println("Cheryl's birthday is " + bstr(filtered4[0]!!))
    } else {
        println("Something went wrong!")
    }
}
