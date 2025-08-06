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

    static Node[] example;

    static int node_sum(Node[] tree, int index) {
        if (index == (-1)) {
            return 0;
        }
        Node node = tree[index];
        return node.value + node_sum(((Node[])(tree)), node.left) + node_sum(((Node[])(tree)), node.right);
    }
    public static void main(String[] args) {
        example = ((Node[])(new Node[]{new Node(10, 1, 2), new Node(5, 3, -1), new Node(-3, 4, 5), new Node(12, -1, -1), new Node(8, -1, -1), new Node(0, -1, -1)}));
        System.out.println(node_sum(((Node[])(example)), 0));
    }
}
