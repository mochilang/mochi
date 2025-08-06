public class Main {
    static class Node {
        int data;
        int prev_index;
        int next_index;
        Node(int data, int prev_index, int next_index) {
            this.data = data;
            this.prev_index = prev_index;
            this.next_index = next_index;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'prev_index': %s, 'next_index': %s}", String.valueOf(data), String.valueOf(prev_index), String.valueOf(next_index));
        }
    }

    static class LinkedList {
        Node[] nodes;
        int head_idx;
        int tail_idx;
        LinkedList(Node[] nodes, int head_idx, int tail_idx) {
            this.nodes = nodes;
            this.head_idx = head_idx;
            this.tail_idx = tail_idx;
        }
        LinkedList() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'head_idx': %s, 'tail_idx': %s}", String.valueOf(nodes), String.valueOf(head_idx), String.valueOf(tail_idx));
        }
    }


    static LinkedList empty_list() {
        return new LinkedList(new Node[]{}, -1, -1);
    }

    static int get_head_data(LinkedList ll) {
        if (ll.head_idx == (-1)) {
            return -1;
        }
        Node node = ll.nodes[ll.head_idx];
        return node.data;
    }

    static int get_tail_data(LinkedList ll) {
        if (ll.tail_idx == (-1)) {
            return -1;
        }
        Node node_1 = ll.nodes[ll.tail_idx];
        return node_1.data;
    }

    static void insert_before_node(LinkedList ll, int idx, int new_idx) {
        Node[] nodes = ((Node[])(ll.nodes));
        Node new_node = nodes[new_idx];
new_node.next_index = idx;
        Node node_2 = nodes[idx];
        int p = node_2.prev_index;
new_node.prev_index = p;
nodes[new_idx] = new_node;
        if (p == (-1)) {
ll.head_idx = new_idx;
        } else {
            Node prev_node = nodes[p];
prev_node.next_index = new_idx;
nodes[p] = prev_node;
        }
node_2.prev_index = new_idx;
nodes[idx] = node_2;
ll.nodes = nodes;
    }

    static void insert_after_node(LinkedList ll, int idx, int new_idx) {
        Node[] nodes_1 = ((Node[])(ll.nodes));
        Node new_node_1 = nodes_1[new_idx];
new_node_1.prev_index = idx;
        Node node_3 = nodes_1[idx];
        int nxt = node_3.next_index;
new_node_1.next_index = nxt;
nodes_1[new_idx] = new_node_1;
        if (nxt == (-1)) {
ll.tail_idx = new_idx;
        } else {
            Node next_node = nodes_1[nxt];
next_node.prev_index = new_idx;
nodes_1[nxt] = next_node;
        }
node_3.next_index = new_idx;
nodes_1[idx] = node_3;
ll.nodes = nodes_1;
    }

    static void set_head(LinkedList ll, int idx) {
        if (ll.head_idx == (-1)) {
ll.head_idx = idx;
ll.tail_idx = idx;
        } else {
            insert_before_node(ll, ll.head_idx, idx);
        }
    }

    static void set_tail(LinkedList ll, int idx) {
        if (ll.tail_idx == (-1)) {
ll.head_idx = idx;
ll.tail_idx = idx;
        } else {
            insert_after_node(ll, ll.tail_idx, idx);
        }
    }

    static void insert(LinkedList ll, int value) {
        Node[] nodes_2 = ((Node[])(ll.nodes));
        nodes_2 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_2), java.util.stream.Stream.of(new Node(value, -1, -1))).toArray(Node[]::new)));
        int idx = nodes_2.length - 1;
ll.nodes = nodes_2;
        if (ll.head_idx == (-1)) {
ll.head_idx = idx;
ll.tail_idx = idx;
        } else {
            insert_after_node(ll, ll.tail_idx, idx);
        }
    }

    static void insert_at_position(LinkedList ll, int position, int value) {
        int current = ll.head_idx;
        int current_pos = 1;
        while (current != (-1)) {
            if (current_pos == position) {
                Node[] nodes_3 = ((Node[])(ll.nodes));
                nodes_3 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_3), java.util.stream.Stream.of(new Node(value, -1, -1))).toArray(Node[]::new)));
                int new_idx = nodes_3.length - 1;
ll.nodes = nodes_3;
                insert_before_node(ll, current, new_idx);
                return;
            }
            Node node_4 = ll.nodes[current];
            current = node_4.next_index;
            current_pos = current_pos + 1;
        }
        insert(ll, value);
    }

    static int get_node(LinkedList ll, int item) {
        int current_1 = ll.head_idx;
        while (current_1 != (-1)) {
            Node node_5 = ll.nodes[current_1];
            if (node_5.data == item) {
                return current_1;
            }
            current_1 = node_5.next_index;
        }
        return -1;
    }

    static void remove_node_pointers(LinkedList ll, int idx) {
        Node[] nodes_4 = ((Node[])(ll.nodes));
        Node node_6 = nodes_4[idx];
        int nxt_1 = node_6.next_index;
        int p_1 = node_6.prev_index;
        if (nxt_1 != (-1)) {
            Node nxt_node = nodes_4[nxt_1];
nxt_node.prev_index = p_1;
nodes_4[nxt_1] = nxt_node;
        }
        if (p_1 != (-1)) {
            Node prev_node_1 = nodes_4[p_1];
prev_node_1.next_index = nxt_1;
nodes_4[p_1] = prev_node_1;
        }
node_6.next_index = -1;
node_6.prev_index = -1;
nodes_4[idx] = node_6;
ll.nodes = nodes_4;
    }

    static void delete_value(LinkedList ll, int value) {
        int idx_1 = get_node(ll, value);
        if (idx_1 == (-1)) {
            return;
        }
        if (idx_1 == ll.head_idx) {
            Node node_7 = ll.nodes[idx_1];
ll.head_idx = node_7.next_index;
        }
        if (idx_1 == ll.tail_idx) {
            Node node_8 = ll.nodes[idx_1];
ll.tail_idx = node_8.prev_index;
        }
        remove_node_pointers(ll, idx_1);
    }

    static boolean contains(LinkedList ll, int value) {
        return get_node(ll, value) != (-1);
    }

    static boolean is_empty(LinkedList ll) {
        return ll.head_idx == (-1);
    }

    static String to_string(LinkedList ll) {
        String res = "";
        boolean first = true;
        int current_2 = ll.head_idx;
        while (current_2 != (-1)) {
            Node node_9 = ll.nodes[current_2];
            String val = _p(node_9.data);
            if (first) {
                res = val;
                first = false;
            } else {
                res = res + " " + val;
            }
            current_2 = node_9.next_index;
        }
        return res;
    }

    static void print_list(LinkedList ll) {
        int current_3 = ll.head_idx;
        while (current_3 != (-1)) {
            Node node_10 = ll.nodes[current_3];
            System.out.println(_p(node_10.data));
            current_3 = node_10.next_index;
        }
    }

    static void main() {
        LinkedList ll = empty_list();
        System.out.println(_p(get_head_data(ll)));
        System.out.println(_p(get_tail_data(ll)));
        System.out.println(_p(is_empty(ll)));
        insert(ll, 10);
        System.out.println(_p(get_head_data(ll)));
        System.out.println(_p(get_tail_data(ll)));
        insert_at_position(ll, 3, 20);
        System.out.println(_p(get_head_data(ll)));
        System.out.println(_p(get_tail_data(ll)));
        Node[] nodes_5 = ((Node[])(ll.nodes));
        nodes_5 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_5), java.util.stream.Stream.of(new Node(1000, -1, -1))).toArray(Node[]::new)));
        int idx_head = nodes_5.length - 1;
ll.nodes = nodes_5;
        set_head(ll, idx_head);
        nodes_5 = ((Node[])(ll.nodes));
        nodes_5 = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes_5), java.util.stream.Stream.of(new Node(2000, -1, -1))).toArray(Node[]::new)));
        int idx_tail = nodes_5.length - 1;
ll.nodes = nodes_5;
        set_tail(ll, idx_tail);
        print_list(ll);
        System.out.println(_p(is_empty(ll)));
        print_list(ll);
        System.out.println(_p(contains(ll, 10)));
        delete_value(ll, 10);
        System.out.println(_p(contains(ll, 10)));
        delete_value(ll, 2000);
        System.out.println(_p(get_tail_data(ll)));
        delete_value(ll, 1000);
        System.out.println(_p(get_tail_data(ll)));
        System.out.println(_p(get_head_data(ll)));
        print_list(ll);
        delete_value(ll, 20);
        print_list(ll);
        int i = 1;
        while (i < 10) {
            insert(ll, i);
            i = i + 1;
        }
        print_list(ll);
        LinkedList ll2 = empty_list();
        insert_at_position(ll2, 1, 10);
        System.out.println(to_string(ll2));
        insert_at_position(ll2, 2, 20);
        System.out.println(to_string(ll2));
        insert_at_position(ll2, 1, 30);
        System.out.println(to_string(ll2));
        insert_at_position(ll2, 3, 40);
        System.out.println(to_string(ll2));
        insert_at_position(ll2, 5, 50);
        System.out.println(to_string(ll2));
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
