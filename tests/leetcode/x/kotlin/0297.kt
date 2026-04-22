import java.util.ArrayDeque

class TreeNode(var `val`: Int) {
    var left: TreeNode? = null
    var right: TreeNode? = null
}

class Codec {
    fun serialize(root: TreeNode?): String {
        if (root == null) return "[]"
        val out = mutableListOf<String>()
        val q = mutableListOf<TreeNode?>()
        q.add(root)
        var head = 0
        while (head < q.size) {
            val node = q[head++]
            if (node == null) {
                out.add("null")
            } else {
                out.add(node.`val`.toString())
                q.add(node.left)
                q.add(node.right)
            }
        }
        while (out.isNotEmpty() && out.last() == "null") out.removeAt(out.lastIndex)
        return "[" + out.joinToString(",") + "]"
    }

    fun deserialize(data: String): TreeNode? {
        if (data == "[]") return null
        val vals = data.substring(1, data.length - 1).split(",")
        val root = TreeNode(vals[0].toInt())
        val q = ArrayDeque<TreeNode>()
        q.add(root)
        var i = 1
        while (q.isNotEmpty() && i < vals.size) {
            val node = q.removeFirst()
            if (i < vals.size && vals[i] != "null") {
                node.left = TreeNode(vals[i].toInt())
                q.add(node.left!!)
            }
            i++
            if (i < vals.size && vals[i] != "null") {
                node.right = TreeNode(vals[i].toInt())
                q.add(node.right!!)
            }
            i++
        }
        return root
    }
}

fun main() {
    val lines = generateSequence { readLine() }.map { it.trim() }.filter { it.isNotEmpty() }.toList()
    if (lines.isEmpty()) return
    val t = lines[0].toInt()
    val codec = Codec()
    val out = mutableListOf<String>()
    repeat(t) { tc ->
        out.add(codec.serialize(codec.deserialize(lines[tc + 1])))
    }
    print(out.joinToString("\n\n"))
}
