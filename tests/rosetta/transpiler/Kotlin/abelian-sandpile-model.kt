val dim: Int = 16
fun newPile(d: Int): MutableList<MutableList<Int>> {
    var b: MutableList<MutableList<Int>> = mutableListOf()
    var y: Int = 0
    while (y < d) {
        var row: MutableList<Int> = mutableListOf()
        var x: Int = 0
        while (x < d) {
            row = run { val _tmp = row.toMutableList(); _tmp.add(0); _tmp } as MutableList<Int>
            x = x + 1
        }
        b = run { val _tmp = b.toMutableList(); _tmp.add(row); _tmp } as MutableList<MutableList<Int>>
        y = y + 1
    }
    return b as MutableList<MutableList<Int>>
}

fun handlePile(pile: MutableList<MutableList<Int>>, x: Int, y: Int): MutableList<MutableList<Int>> {
    var pile: MutableList<MutableList<Int>> = pile
    if (pile[y][x] >= 4) {
        pile[y][x] = pile[y][x] - 4
        if (y > 0) {
            pile[y - 1][x] = pile[y - 1][x] + 1
            if (pile[y - 1][x] >= 4) {
                pile = handlePile(pile, x, y - 1) as MutableList<MutableList<Int>>
            }
        }
        if (x > 0) {
            pile[y][x - 1] = pile[y][x - 1] + 1
            if (pile[y][x - 1] >= 4) {
                pile = handlePile(pile, x - 1, y) as MutableList<MutableList<Int>>
            }
        }
        if (y < (dim - 1)) {
            pile[y + 1][x] = pile[y + 1][x] + 1
            if (pile[y + 1][x] >= 4) {
                pile = handlePile(pile, x, y + 1) as MutableList<MutableList<Int>>
            }
        }
        if (x < (dim - 1)) {
            pile[y][x + 1] = pile[y][x + 1] + 1
            if (pile[y][x + 1] >= 4) {
                pile = handlePile(pile, x + 1, y) as MutableList<MutableList<Int>>
            }
        }
        pile = handlePile(pile, x, y) as MutableList<MutableList<Int>>
    }
    return pile as MutableList<MutableList<Int>>
}

fun drawPile(pile: MutableList<MutableList<Int>>, d: Int): Unit {
    val chars: MutableList<String> = mutableListOf(" ", "░", "▓", "█")
    var row: Int = 0
    while (row < d) {
        var line: String = ""
        var col: Int = 0
        while (col < d) {
            var v: Int = pile[row][col]
            if (v > 3) {
                v = 3
            }
            line = line + chars[v]
            col = col + 1
        }
        println(line)
        row = row + 1
    }
}

fun user_main(): Unit {
    var pile: MutableList<MutableList<Int>> = newPile(16) as MutableList<MutableList<Int>>
    val hdim: Int = 7
    pile[hdim][hdim] = 16
    pile = handlePile(pile as MutableList<MutableList<Int>>, hdim, hdim) as MutableList<MutableList<Int>> as MutableList<Any>
    drawPile(pile as MutableList<MutableList<Int>>, 16)
}

fun main() {
    user_main()
}
