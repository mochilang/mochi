public class Main {
    static class Node {
        String data;
        int prev;
        int next;
        Node(String data, int prev, int next) {
            this.data = data;
            this.prev = prev;
            this.next = next;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'data': '%s', 'prev': %s, 'next': %s}", String.valueOf(data), String.valueOf(prev), String.valueOf(next));
        }
    }

    static class LinkedDeque {
        Node[] nodes;
        int header;
        int trailer;
        int size;
        LinkedDeque(Node[] nodes, int header, int trailer, int size) {
            this.nodes = nodes;
            this.header = header;
            this.trailer = trailer;
            this.size = size;
        }
        LinkedDeque() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'header': %s, 'trailer': %s, 'size': %s}", String.valueOf(nodes), String.valueOf(header), String.valueOf(trailer), String.valueOf(size));
        }
    }

    static class DeleteResult {
        LinkedDeque deque;
        String value;
        DeleteResult(LinkedDeque deque, String value) {
            this.deque = deque;
            this.value = value;
        }
        DeleteResult() {}
        @Override public String toString() {
            return String.format("{'deque': %s, 'value': '%s'}", String.valueOf(deque), String.valueOf(value));
        }
    }


    static LinkedDeque new_deque() {
        Node[] nodes = ((Node[])(new Node[]{}));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new Node("", -1, 1))).toArray(Node[]::new)));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new Node("", 0, -1))).toArray(Node[]::new)));
        return new LinkedDeque(nodes, 0, 1, 0);
    }

    static boolean is_empty(LinkedDeque d) {
        return d.size == 0;
    }

    static String front(LinkedDeque d) {
        if (((Boolean)(is_empty(d)))) {
            throw new RuntimeException(String.valueOf("List is empty"));
        }
        Node head = d.nodes[d.header];
        int idx = head.next;
        Node node = d.nodes[idx];
        return node.data;
    }

    static String back(LinkedDeque d) {
        if (((Boolean)(is_empty(d)))) {
            throw new RuntimeException(String.valueOf("List is empty"));
        }
        Node tail = d.nodes[d.trailer];
        int idx_1 = tail.prev;
        Node node_1 = d.nodes[idx_1];
        return node_1.data;
    }

    static LinkedDeque insert(LinkedDeque d, int pred, String value, int succ) {
        Node[] nodes_1 = ((Node[])(d.nodes));
        int new_idx = nodes_1.length;
        nodes_1 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_1), java.util.stream.Stream.of(new Node(value, pred, succ))).toArray(Node[]::new)));
        Node pred_node = nodes_1[pred];
pred_node.next = new_idx;
nodes_1[pred] = pred_node;
        Node succ_node = nodes_1[succ];
succ_node.prev = new_idx;
nodes_1[succ] = succ_node;
d.nodes = nodes_1;
d.size = d.size + 1;
        return d;
    }

    static DeleteResult delete(LinkedDeque d, int idx) {
        Node[] nodes_2 = ((Node[])(d.nodes));
        Node node_2 = nodes_2[idx];
        int pred = node_2.prev;
        int succ = node_2.next;
        Node pred_node_1 = nodes_2[pred];
pred_node_1.next = succ;
nodes_2[pred] = pred_node_1;
        Node succ_node_1 = nodes_2[succ];
succ_node_1.prev = pred;
nodes_2[succ] = succ_node_1;
        String val = node_2.data;
d.nodes = nodes_2;
d.size = d.size - 1;
        return new DeleteResult(d, val);
    }

    static LinkedDeque add_first(LinkedDeque d, String value) {
        Node head_1 = d.nodes[d.header];
        int succ_1 = head_1.next;
        return insert(d, d.header, value, succ_1);
    }

    static LinkedDeque add_last(LinkedDeque d, String value) {
        Node tail_1 = d.nodes[d.trailer];
        int pred_1 = tail_1.prev;
        return insert(d, pred_1, value, d.trailer);
    }

    static DeleteResult remove_first(LinkedDeque d) {
        if (((Boolean)(is_empty(d)))) {
            throw new RuntimeException(String.valueOf("remove_first from empty list"));
        }
        Node head_2 = d.nodes[d.header];
        int idx_2 = head_2.next;
        return delete(d, idx_2);
    }

    static DeleteResult remove_last(LinkedDeque d) {
        if (((Boolean)(is_empty(d)))) {
            throw new RuntimeException(String.valueOf("remove_first from empty list"));
        }
        Node tail_2 = d.nodes[d.trailer];
        int idx_3 = tail_2.prev;
        return delete(d, idx_3);
    }

    static void main() {
        LinkedDeque d = new_deque();
        d = add_first(d, "A");
        System.out.println(front(d));
        d = add_last(d, "B");
        System.out.println(back(d));
        DeleteResult r = remove_first(d);
        d = r.deque;
        System.out.println(r.value);
        r = remove_last(d);
        d = r.deque;
        System.out.println(r.value);
        System.out.println(_p(is_empty(d)));
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
