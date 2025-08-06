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

    static class TreeState {
        Node[] nodes;
        int root;
        TreeState(Node[] nodes, int root) {
            this.nodes = nodes;
            this.root = root;
        }
        TreeState() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'root': %s}", String.valueOf(nodes), String.valueOf(root));
        }
    }


    static int new_node(TreeState state, int value) {
state.nodes = java.util.stream.Stream.concat(java.util.Arrays.stream(state.nodes), java.util.stream.Stream.of(new Node(value, (-1), (-1)))).toArray(Node[]::new);
        return state.nodes.length - 1;
    }

    static void insert(TreeState state, int value) {
        if (state.root == (-1)) {
state.root = new_node(state, value);
            return;
        }
        int current = state.root;
        Node[] nodes = ((Node[])(state.nodes));
        while (true) {
            Node node = nodes[current];
            if (value < node.data) {
                if (node.left == (-1)) {
node.left = new_node(state, value);
nodes[current] = node;
state.nodes = nodes;
                    return;
                }
                current = node.left;
            } else {
                if (node.right == (-1)) {
node.right = new_node(state, value);
nodes[current] = node;
state.nodes = nodes;
                    return;
                }
                current = node.right;
            }
        }
    }

    static int[] inorder(TreeState state, int idx) {
        if (idx == (-1)) {
            return new int[]{};
        }
        Node node_1 = state.nodes[idx];
        int[] result = ((int[])(inorder(state, node_1.left)));
        result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(node_1.data)).toArray()));
        int[] right_part = ((int[])(inorder(state, node_1.right)));
        int i = 0;
        while (i < right_part.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(right_part[i])).toArray()));
            i = i + 1;
        }
        return result;
    }

    static TreeState make_tree() {
        TreeState state = new TreeState(new Node[]{}, (-1));
        insert(state, 15);
        insert(state, 10);
        insert(state, 25);
        insert(state, 6);
        insert(state, 14);
        insert(state, 20);
        insert(state, 60);
        return state;
    }

    static void main() {
        TreeState state_1 = make_tree();
        System.out.println("Printing values of binary search tree in Inorder Traversal.");
        System.out.println(inorder(state_1, state_1.root));
    }
    public static void main(String[] args) {
        main();
    }
}
