public class Main {
    static class TreeNode {
        int data;
        int left;
        int right;
        TreeNode(int data, int left, int right) {
            this.data = data;
            this.left = left;
            this.right = right;
        }
        TreeNode() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'left': %s, 'right': %s}", String.valueOf(data), String.valueOf(left), String.valueOf(right));
        }
    }

    static int total_moves = 0;

    static int count_nodes(TreeNode[] nodes, int idx) {
        if (idx == 0) {
            return 0;
        }
        TreeNode node = nodes[idx];
        return count_nodes(((TreeNode[])(nodes)), node.left) + count_nodes(((TreeNode[])(nodes)), node.right) + 1;
    }

    static int count_coins(TreeNode[] nodes, int idx) {
        if (idx == 0) {
            return 0;
        }
        TreeNode node_1 = nodes[idx];
        return count_coins(((TreeNode[])(nodes)), node_1.left) + count_coins(((TreeNode[])(nodes)), node_1.right) + node_1.data;
    }

    static int iabs(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static int dfs(TreeNode[] nodes, int idx) {
        if (idx == 0) {
            return 0;
        }
        TreeNode node_2 = nodes[idx];
        int left_excess = dfs(((TreeNode[])(nodes)), node_2.left);
        int right_excess = dfs(((TreeNode[])(nodes)), node_2.right);
        int abs_left = iabs(left_excess);
        int abs_right = iabs(right_excess);
        total_moves = total_moves + abs_left + abs_right;
        return node_2.data + left_excess + right_excess - 1;
    }

    static int distribute_coins(TreeNode[] nodes, int root) {
        if (root == 0) {
            return 0;
        }
        if (count_nodes(((TreeNode[])(nodes)), root) != count_coins(((TreeNode[])(nodes)), root)) {
            throw new RuntimeException(String.valueOf("The nodes number should be same as the number of coins"));
        }
        total_moves = 0;
        dfs(((TreeNode[])(nodes)), root);
        return total_moves;
    }

    static void main() {
        TreeNode[] example1 = ((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0), new TreeNode(3, 2, 3), new TreeNode(0, 0, 0), new TreeNode(0, 0, 0)}));
        TreeNode[] example2 = ((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0), new TreeNode(0, 2, 3), new TreeNode(3, 0, 0), new TreeNode(0, 0, 0)}));
        TreeNode[] example3 = ((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0), new TreeNode(0, 2, 3), new TreeNode(0, 0, 0), new TreeNode(3, 0, 0)}));
        System.out.println(distribute_coins(((TreeNode[])(example1)), 1));
        System.out.println(distribute_coins(((TreeNode[])(example2)), 1));
        System.out.println(distribute_coins(((TreeNode[])(example3)), 1));
        System.out.println(distribute_coins(((TreeNode[])(new TreeNode[]{new TreeNode(0, 0, 0)})), 0));
    }
    public static void main(String[] args) {
        total_moves = 0;
        main();
    }
}
