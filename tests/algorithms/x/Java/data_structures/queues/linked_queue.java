public class Main {
    static class Node {
        String data;
        int next;
        Node(String data, int next) {
            this.data = data;
            this.next = next;
        }
        Node() {}
        @Override public String toString() {
            return String.format("{'data': '%s', 'next': %s}", String.valueOf(data), String.valueOf(next));
        }
    }

    static class LinkedQueue {
        Node[] nodes;
        int front;
        int rear;
        LinkedQueue(Node[] nodes, int front, int rear) {
            this.nodes = nodes;
            this.front = front;
            this.rear = rear;
        }
        LinkedQueue() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'front': %s, 'rear': %s}", String.valueOf(nodes), String.valueOf(front), String.valueOf(rear));
        }
    }

    static LinkedQueue queue;

    static LinkedQueue new_queue() {
        return new LinkedQueue(new Node[]{}, 0 - 1, 0 - 1);
    }

    static boolean is_empty(LinkedQueue q) {
        return q.front == 0 - 1;
    }

    static void put(LinkedQueue q, String item) {
        Node node = new Node(item, 0 - 1);
q.nodes = java.util.stream.Stream.concat(java.util.Arrays.stream(q.nodes), java.util.stream.Stream.of(node)).toArray(Node[]::new);
        int idx = q.nodes.length - 1;
        if (q.front == 0 - 1) {
q.front = idx;
q.rear = idx;
        } else {
            Node[] nodes = ((Node[])(q.nodes));
q.nodes = nodes;
q.rear = idx;
        }
    }

    static String get(LinkedQueue q) {
        if (((Boolean)(is_empty(q)))) {
            throw new RuntimeException(String.valueOf("dequeue from empty queue"));
        }
        int idx_1 = q.front;
        Node node_1 = q.nodes[idx_1];
q.front = node_1.next;
        if (q.front == 0 - 1) {
q.rear = 0 - 1;
        }
        return node_1.data;
    }

    static int length(LinkedQueue q) {
        int count = 0;
        int idx_2 = q.front;
        while (idx_2 != 0 - 1) {
            count = count + 1;
            idx_2 = q.nodes[idx_2].next;
        }
        return count;
    }

    static String to_string(LinkedQueue q) {
        String res = "";
        int idx_3 = q.front;
        boolean first = true;
        while (idx_3 != 0 - 1) {
            Node node_2 = q.nodes[idx_3];
            if (first) {
                res = node_2.data;
                first = false;
            } else {
                res = res + " <- " + node_2.data;
            }
            idx_3 = node_2.next;
        }
        return res;
    }

    static void clear(LinkedQueue q) {
q.nodes = new Object[]{};
q.front = 0 - 1;
q.rear = 0 - 1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            queue = new_queue();
            System.out.println(_p(is_empty(queue)));
            put(queue, "5");
            put(queue, "9");
            put(queue, "python");
            System.out.println(_p(is_empty(queue)));
            System.out.println(get(queue));
            put(queue, "algorithms");
            System.out.println(get(queue));
            System.out.println(get(queue));
            System.out.println(get(queue));
            System.out.println(_p(is_empty(queue)));
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
