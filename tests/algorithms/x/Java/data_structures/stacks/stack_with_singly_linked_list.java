public class Main {
    static class Node {
        String value;
        int next;
        Node(String value, int next) {
            this.value = value;
            this.next = next;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'value': '%s', 'next': %s}", String.valueOf(value), String.valueOf(next));
        }
    }

    static class Stack {
        Node[] nodes;
        int top;
        Stack(Node[] nodes, int top) {
            this.nodes = nodes;
            this.top = top;
        }
        Stack() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'top': %s}", String.valueOf(nodes), String.valueOf(top));
        }
    }

    static class PopResult {
        Stack stack;
        String value;
        PopResult(Stack stack, String value) {
            this.stack = stack;
            this.value = value;
        }
        PopResult() {}
        @Override public String toString() {
            return String.format("{'stack': %s, 'value': '%s'}", String.valueOf(stack), String.valueOf(value));
        }
    }


    static Stack empty_stack() {
        return new Stack(new Node[]{}, (-1));
    }

    static boolean is_empty(Stack stack) {
        return stack.top == (-1);
    }

    static Stack push(Stack stack, String item) {
        Node new_node = new Node(item, stack.top);
        Node[] new_nodes = ((Node[])(stack.nodes));
        new_nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_nodes), java.util.stream.Stream.of(new_node)).toArray(Node[]::new)));
        int new_top = new_nodes.length - 1;
        return new Stack(new_nodes, new_top);
    }

    static PopResult pop(Stack stack) {
        if (stack.top == (-1)) {
            throw new RuntimeException(String.valueOf("pop from empty stack"));
        }
        Node node = (stack.nodes[stack.top]);
        int new_top_1 = node.next;
        Stack new_stack = new Stack(stack.nodes, new_top_1);
        return new PopResult(new_stack, node.value);
    }

    static String peek(Stack stack) {
        if (stack.top == (-1)) {
            throw new RuntimeException(String.valueOf("peek from empty stack"));
        }
        Node node_1 = (stack.nodes[stack.top]);
        return node_1.value;
    }

    static Stack clear(Stack stack) {
        return new Stack(new Node[]{}, (-1));
    }

    static void main() {
        Stack stack = empty_stack();
        System.out.println(is_empty(stack));
        stack = push(stack, "5");
        stack = push(stack, "9");
        stack = push(stack, "python");
        System.out.println(is_empty(stack));
        PopResult res = pop(stack);
        stack = res.stack;
        System.out.println(res.value);
        stack = push(stack, "algorithms");
        res = pop(stack);
        stack = res.stack;
        System.out.println(res.value);
        res = pop(stack);
        stack = res.stack;
        System.out.println(res.value);
        res = pop(stack);
        stack = res.stack;
        System.out.println(res.value);
        System.out.println(is_empty(stack));
    }
    public static void main(String[] args) {
        main();
    }
}
