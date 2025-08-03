import java.math.BigInteger

var width: Int = 60
var height: Int = ((((width.toDouble())) * 0.86602540378).toInt())
var iterations: Int = 5000
var grid: MutableList<MutableList<String>> = mutableListOf<MutableList<String>>()
var y: Int = 0
var seed: Int = 1
var vertices: MutableList<MutableList<Int>> = mutableListOf(mutableListOf(0, height - 1), mutableListOf(width - 1, height - 1), mutableListOf(((width / 2).toInt()), 0))
var px: Int = ((width / 2).toInt())
var py: Int = ((height / 2).toInt())
var i: Int = 0
fun randInt(s: Int, n: Int): MutableList<Int> {
    var next: BigInteger = (Math.floorMod(((s * 1664525) + 1013904223), 2147483647)).toBigInteger()
    return mutableListOf<Int>((next.toInt()), ((next.remainder((n).toBigInteger())).toInt()))
}

fun main() {
    while (y < height) {
        var line: MutableList<String> = mutableListOf<String>()
        var x: Int = 0
        while (x < width) {
            line = run { val _tmp = line.toMutableList(); _tmp.add(" "); _tmp }
            x = x + 1
        }
        grid = run { val _tmp = grid.toMutableList(); _tmp.add(line); _tmp }
        y = y + 1
    }
    while (i < iterations) {
        var r: MutableList<Int> = randInt(seed, 3)
        seed = r[0]!!
        var idx: Int = ((r[1]!!).toInt())
        var v: MutableList<Int> = vertices[idx]!!
        px = (((px + v[0]!!) / 2).toInt())
        py = (((py + v[1]!!) / 2).toInt())
        if ((((((px >= 0) && (px < width) as Boolean)) && (py >= 0) as Boolean)) && (py < height)) {
            (grid[py]!!)[px] = "*"
        }
        i = i + 1
    }
    y = 0
    while (y < height) {
        var line: String = ""
        var x: Int = 0
        while (x < width) {
            line = line + (((grid[y]!!) as MutableList<String>))[x]!!
            x = x + 1
        }
        println(line)
        y = y + 1
    }
}
