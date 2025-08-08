fun panic(msg: String): Nothing { throw RuntimeException(msg) }

data class TreeNode(var data: Int = 0, var left: Int = 0, var right: Int = 0)
var total_moves: Int = (0).toInt()
fun count_nodes(nodes: MutableList<TreeNode>, idx: Int): Int {
    if (idx == 0) {
        return 0
    }
    var node: TreeNode = nodes[idx]!!
    return (count_nodes(nodes, node.left) + count_nodes(nodes, node.right)) + 1
}

fun count_coins(nodes: MutableList<TreeNode>, idx: Int): Int {
    if (idx == 0) {
        return 0
    }
    var node: TreeNode = nodes[idx]!!
    return (count_coins(nodes, node.left) + count_coins(nodes, node.right)) + node.data
}

fun iabs(x: Int): Int {
    if (x < 0) {
        return 0 - x
    }
    return x
}

fun dfs(nodes: MutableList<TreeNode>, idx: Int): Int {
    if (idx == 0) {
        return 0
    }
    var node: TreeNode = nodes[idx]!!
    var left_excess: Int = (dfs(nodes, node.left)).toInt()
    var right_excess: Int = (dfs(nodes, node.right)).toInt()
    var abs_left: Int = (iabs(left_excess)).toInt()
    var abs_right: Int = (iabs(right_excess)).toInt()
    total_moves = ((total_moves + abs_left) + abs_right).toInt()
    return ((node.data + left_excess) + right_excess) - 1
}

fun distribute_coins(nodes: MutableList<TreeNode>, root: Int): Int {
    if (root == 0) {
        return 0
    }
    if (count_nodes(nodes, root) != count_coins(nodes, root)) {
        panic("The nodes number should be same as the number of coins")
    }
    total_moves = (0).toInt()
    dfs(nodes, root)
    return total_moves
}

fun user_main(): Unit {
    var example1: MutableList<TreeNode> = mutableListOf(TreeNode(data = 0, left = 0, right = 0), TreeNode(data = 3, left = 2, right = 3), TreeNode(data = 0, left = 0, right = 0), TreeNode(data = 0, left = 0, right = 0))
    var example2: MutableList<TreeNode> = mutableListOf(TreeNode(data = 0, left = 0, right = 0), TreeNode(data = 0, left = 2, right = 3), TreeNode(data = 3, left = 0, right = 0), TreeNode(data = 0, left = 0, right = 0))
    var example3: MutableList<TreeNode> = mutableListOf(TreeNode(data = 0, left = 0, right = 0), TreeNode(data = 0, left = 2, right = 3), TreeNode(data = 0, left = 0, right = 0), TreeNode(data = 3, left = 0, right = 0))
    println(distribute_coins(example1, 1))
    println(distribute_coins(example2, 1))
    println(distribute_coins(example3, 1))
    println(distribute_coins(mutableListOf(TreeNode(data = 0, left = 0, right = 0)), 0))
}

fun main() {
    user_main()
}
