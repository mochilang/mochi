public class Main {
    static class Node {
        int data;
        int left;
        int right;
        Node(int data, int left, int right) {
            this.data = data;
            this.left = left;
            this.right = right;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'left': %s, 'right': %s}", String.valueOf(data), String.valueOf(left), String.valueOf(right));
        }
    }

    static Node[] small;
    static Node[] medium;

    static int[] inorder(Node[] nodes, int index, int[] acc) {
        if (index == 0 - 1) {
            return acc;
        }
        Node node = nodes[index];
        int[] res = ((int[])(inorder(((Node[])(nodes)), node.left, ((int[])(acc)))));
        res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(node.data)).toArray()));
        res = ((int[])(inorder(((Node[])(nodes)), node.right, ((int[])(res)))));
        return res;
    }

    static int size(Node[] nodes, int index) {
        if (index == 0 - 1) {
            return 0;
        }
        Node node_1 = nodes[index];
        return 1 + size(((Node[])(nodes)), node_1.left) + size(((Node[])(nodes)), node_1.right);
    }

    static int depth(Node[] nodes, int index) {
        if (index == 0 - 1) {
            return 0;
        }
        Node node_2 = nodes[index];
        int left_depth = depth(((Node[])(nodes)), node_2.left);
        int right_depth = depth(((Node[])(nodes)), node_2.right);
        if (left_depth > right_depth) {
            return left_depth + 1;
        }
        return right_depth + 1;
    }

    static boolean is_full(Node[] nodes, int index) {
        if (index == 0 - 1) {
            return true;
        }
        Node node_3 = nodes[index];
        if (node_3.left == 0 - 1 && node_3.right == 0 - 1) {
            return true;
        }
        if (node_3.left != 0 - 1 && node_3.right != 0 - 1) {
            return ((Boolean)(is_full(((Node[])(nodes)), node_3.left))) && ((Boolean)(is_full(((Node[])(nodes)), node_3.right)));
        }
        return false;
    }

    static Node[] small_tree() {
        Node[] arr = ((Node[])(new Node[]{}));
        arr = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new Node(2, 1, 2))).toArray(Node[]::new)));
        arr = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new Node(1, 0 - 1, 0 - 1))).toArray(Node[]::new)));
        arr = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(new Node(3, 0 - 1, 0 - 1))).toArray(Node[]::new)));
        return arr;
    }

    static Node[] medium_tree() {
        Node[] arr_1 = ((Node[])(new Node[]{}));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(4, 1, 4))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(2, 2, 3))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(1, 0 - 1, 0 - 1))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(3, 0 - 1, 0 - 1))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(5, 0 - 1, 5))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(6, 0 - 1, 6))).toArray(Node[]::new)));
        arr_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr_1), java.util.stream.Stream.of(new Node(7, 0 - 1, 0 - 1))).toArray(Node[]::new)));
        return arr_1;
    }
    public static void main(String[] args) {
        small = ((Node[])(small_tree()));
        System.out.println(size(((Node[])(small)), 0));
        System.out.println(inorder(((Node[])(small)), 0, ((int[])(new int[]{}))));
        System.out.println(depth(((Node[])(small)), 0));
        System.out.println(is_full(((Node[])(small)), 0));
        medium = ((Node[])(medium_tree()));
        System.out.println(size(((Node[])(medium)), 0));
        System.out.println(inorder(((Node[])(medium)), 0, ((int[])(new int[]{}))));
        System.out.println(depth(((Node[])(medium)), 0));
        System.out.println(is_full(((Node[])(medium)), 0));
    }
}
