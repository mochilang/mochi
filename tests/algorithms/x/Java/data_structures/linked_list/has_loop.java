public class Main {
    static class Node {
        int data;
        int next;
        Node(int data, int next) {
            this.data = data;
            this.next = next;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'next': %s}", String.valueOf(data), String.valueOf(next));
        }
    }


    static boolean has_loop(Node[] nodes, int head) {
        int slow = head;
        int fast = head;
        while (fast != 0 - 1) {
            Node fast_node1 = nodes[fast];
            if (fast_node1.next == 0 - 1) {
                return false;
            }
            Node fast_node2 = nodes[fast_node1.next];
            if (fast_node2.next == 0 - 1) {
                return false;
            }
            Node slow_node = nodes[slow];
            slow = slow_node.next;
            fast = fast_node2.next;
            if (slow == fast) {
                return true;
            }
        }
        return false;
    }

    static Node[] make_nodes(int[] values) {
        Node[] nodes = ((Node[])(new Node[]{}));
        int i = 0;
        while (i < values.length) {
            int next_idx = i == values.length - 1 ? 0 - 1 : i + 1;
            nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new Node(values[i], next_idx))).toArray(Node[]::new)));
            i = i + 1;
        }
        return nodes;
    }

    static void main() {
        Node[] list1 = ((Node[])(make_nodes(((int[])(new int[]{1, 2, 3, 4})))));
        System.out.println(_p(has_loop(((Node[])(list1)), 0)));
        System.out.println(_p(has_loop(((Node[])(list1)), 0)));
        Node[] list2 = ((Node[])(make_nodes(((int[])(new int[]{5, 6, 5, 6})))));
        System.out.println(_p(has_loop(((Node[])(list2)), 0)));
        Node[] list3 = ((Node[])(make_nodes(((int[])(new int[]{1})))));
        System.out.println(_p(has_loop(((Node[])(list3)), 0)));
    }
    public static void main(String[] args) {
        main();
    }

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
