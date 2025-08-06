public class Main {
    static class Node {
        int data;
        int next;
        int prev;
        Node(int data, int next, int prev) {
            this.data = data;
            this.next = next;
            this.prev = prev;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'next': %s, 'prev': %s}", String.valueOf(data), String.valueOf(next), String.valueOf(prev));
        }
    }

    static class Stack {
        Node[] nodes;
        int head;
        Stack(Node[] nodes, int head) {
            this.nodes = nodes;
            this.head = head;
        }
        Stack() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'head': %s}", String.valueOf(nodes), String.valueOf(head));
        }
    }

    static class PopResult {
        Stack stack;
        int value;
        boolean ok;
        PopResult(Stack stack, int value, boolean ok) {
            this.stack = stack;
            this.value = value;
            this.ok = ok;
        }
        PopResult() {}
        @Override public String toString() {
            return String.format("{'stack': %s, 'value': %s, 'ok': %s}", String.valueOf(stack), String.valueOf(value), String.valueOf(ok));
        }
    }

    static class TopResult {
        int value;
        boolean ok;
        TopResult(int value, boolean ok) {
            this.value = value;
            this.ok = ok;
        }
        TopResult() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'ok': %s}", String.valueOf(value), String.valueOf(ok));
        }
    }


    static Stack empty_stack() {
        return new Stack(new Node[]{}, 0 - 1);
    }

    static Stack push(Stack stack, int value) {
        Node[] nodes = ((Node[])(stack.nodes));
        int idx = nodes.length;
        Node new_node = new Node(value, stack.head, 0 - 1);
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new_node)).toArray(Node[]::new)));
        if (stack.head != 0 - 1) {
            Node head_node = nodes[stack.head];
head_node.prev = idx;
nodes[stack.head] = head_node;
        }
        return new Stack(nodes, idx);
    }

    static PopResult pop(Stack stack) {
        if (stack.head == 0 - 1) {
            return new PopResult(stack, 0, false);
        }
        Node[] nodes_1 = ((Node[])(stack.nodes));
        Node head_node_1 = nodes_1[stack.head];
        int value = head_node_1.data;
        int next_idx = head_node_1.next;
        if (next_idx != 0 - 1) {
            Node next_node = nodes_1[next_idx];
next_node.prev = 0 - 1;
nodes_1[next_idx] = next_node;
        }
        Stack new_stack = new Stack(nodes_1, next_idx);
        return new PopResult(new_stack, value, true);
    }

    static TopResult top(Stack stack) {
        if (stack.head == 0 - 1) {
            return new TopResult(0, false);
        }
        Node node = stack.nodes[stack.head];
        return new TopResult(node.data, true);
    }

    static int size(Stack stack) {
        int count = 0;
        int idx_1 = stack.head;
        while (idx_1 != 0 - 1) {
            count = count + 1;
            Node node_1 = stack.nodes[idx_1];
            idx_1 = node_1.next;
        }
        return count;
    }

    static boolean is_empty(Stack stack) {
        return stack.head == 0 - 1;
    }

    static void print_stack(Stack stack) {
        System.out.println("stack elements are:");
        int idx_2 = stack.head;
        String s = "";
        while (idx_2 != 0 - 1) {
            Node node_2 = stack.nodes[idx_2];
            s = s + _p(node_2.data) + "->";
            idx_2 = node_2.next;
        }
        if (_runeLen(s) > 0) {
            System.out.println(s);
        }
    }

    static void main() {
        Stack stack = empty_stack();
        System.out.println("Stack operations using Doubly LinkedList");
        stack = push(stack, 4);
        stack = push(stack, 5);
        stack = push(stack, 6);
        stack = push(stack, 7);
        print_stack(stack);
        TopResult t = top(stack);
        if (t.ok) {
            System.out.println("Top element is " + _p(t.value));
        } else {
            System.out.println("Top element is None");
        }
        System.out.println("Size of the stack is " + _p(size(stack)));
        PopResult p = pop(stack);
        stack = p.stack;
        p = pop(stack);
        stack = p.stack;
        print_stack(stack);
        System.out.println("stack is empty: " + _p(is_empty(stack)));
    }
    public static void main(String[] args) {
        main();
    }

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
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
