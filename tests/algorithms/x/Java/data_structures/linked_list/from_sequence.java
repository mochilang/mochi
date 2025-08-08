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

    static int NIL;
    static Node[] nodes = new Node[0];

    static int make_linked_list(int[] elements) {
        if (elements.length == 0) {
            throw new RuntimeException(String.valueOf("The Elements List is empty"));
        }
        nodes = ((Node[])(new Node[]{}));
        nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new Node(elements[0], NIL))).toArray(Node[]::new)));
        int head = 0;
        int current = head;
        int i = 1;
        while (i < elements.length) {
            nodes = ((Node[])(java.util.stream.Stream.concat(java.util.Arrays.stream(nodes), java.util.stream.Stream.of(new Node(elements[i], NIL))).toArray(Node[]::new)));
            current = nodes.length - 1;
            i = i + 1;
        }
        return head;
    }

    static String node_to_string(int head) {
        String s = "";
        int index = head;
        while (index != NIL) {
            Node node = nodes[index];
            s = s + "<" + _p(node.data) + "> ---> ";
            index = node.next;
        }
        s = s + "<END>";
        return s;
    }

    static void main() {
        int[] list_data = ((int[])(new int[]{1, 3, 5, 32, 44, 12, 43}));
        System.out.println("List: " + _p(list_data));
        System.out.println("Creating Linked List from List.");
        int head_1 = make_linked_list(((int[])(list_data)));
        System.out.println("Linked List:");
        System.out.println(node_to_string(head_1));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NIL = 0 - 1;
            nodes = ((Node[])(new Node[]{}));
            main();
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
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
