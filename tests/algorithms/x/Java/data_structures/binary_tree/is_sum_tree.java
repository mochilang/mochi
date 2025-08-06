public class Main {
    static class Node {
        int value;
        int left;
        int right;
        Node(int value, int left, int right) {
            this.value = value;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'left': %s, 'right': %s}", String.valueOf(value), String.valueOf(left), String.valueOf(right));
        }
    }


    static int tree_sum(Node[] nodes, int idx) {
        if (idx == (-1)) {
            return 0;
        }
        Node node = nodes[idx];
        return node.value + tree_sum(((Node[])(nodes)), node.left) + tree_sum(((Node[])(nodes)), node.right);
    }

    static boolean is_sum_node(Node[] nodes, int idx) {
        Node node_1 = nodes[idx];
        if (node_1.left == (-1) && node_1.right == (-1)) {
            return true;
        }
        int left_sum = tree_sum(((Node[])(nodes)), node_1.left);
        int right_sum = tree_sum(((Node[])(nodes)), node_1.right);
        if (node_1.value != left_sum + right_sum) {
            return false;
        }
        boolean left_ok = true;
        if (node_1.left != (-1)) {
            left_ok = ((Boolean)(is_sum_node(((Node[])(nodes)), node_1.left)));
        }
        boolean right_ok = true;
        if (node_1.right != (-1)) {
            right_ok = ((Boolean)(is_sum_node(((Node[])(nodes)), node_1.right)));
        }
        return left_ok && right_ok;
    }

    static Node[] build_a_tree() {
        return new Node[]{new Node(11, 1, 2), new Node(2, 3, 4), new Node(29, 5, 6), new Node(1, (-1), (-1)), new Node(7, (-1), (-1)), new Node(15, (-1), (-1)), new Node(40, 7, (-1)), new Node(35, (-1), (-1))};
    }

    static Node[] build_a_sum_tree() {
        return new Node[]{new Node(26, 1, 2), new Node(10, 3, 4), new Node(3, (-1), 5), new Node(4, (-1), (-1)), new Node(6, (-1), (-1)), new Node(3, (-1), (-1))};
    }
    public static void main(String[] args) {
    }
}
