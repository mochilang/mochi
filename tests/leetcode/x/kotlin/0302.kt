fun minArea(image: List<String>, x: Int, y: Int): Int {
    var top = image.size
    var bottom = -1
    var left = image[0].length
    var right = -1
    for (i in image.indices) {
        for (j in image[i].indices) {
            if (image[i][j] == '1') {
                top = minOf(top, i)
                bottom = maxOf(bottom, i)
                left = minOf(left, j)
                right = maxOf(right, j)
            }
        }
    }
    return (bottom - top + 1) * (right - left + 1)
}

fun main() {
    val lines = generateSequence { readLine() }.map { it.trim() }.filter { it.isNotEmpty() }.toList()
    if (lines.isEmpty()) return
    val t = lines[0].toInt()
    var idx = 1
    val blocks = mutableListOf<String>()
    repeat(t) {
        val parts = lines[idx++].split(Regex("\\s+"))
        val r = parts[0].toInt()
        val image = lines.subList(idx, idx + r)
        idx += r
        val coords = lines[idx++].split(Regex("\\s+"))
        blocks.add(minArea(image, coords[0].toInt(), coords[1].toInt()).toString())
    }
    print(blocks.joinToString("\n\n"))
}
