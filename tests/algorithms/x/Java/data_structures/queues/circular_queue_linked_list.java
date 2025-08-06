public class Main {
    static class CircularQueue {
        String[] data;
        int[] next;
        int[] prev;
        int front;
        int rear;
        CircularQueue(String[] data, int[] next, int[] prev, int front, int rear) {
            this.data = data;
            this.next = next;
            this.prev = prev;
            this.front = front;
            this.rear = rear;
        }
        CircularQueue() {}
        @Override public String toString() {
            return String.format("{'data': %s, 'next': %s, 'prev': %s, 'front': %s, 'rear': %s}", String.valueOf(data), String.valueOf(next), String.valueOf(prev), String.valueOf(front), String.valueOf(rear));
        }
    }

    static class DequeueResult {
        CircularQueue queue;
        String value;
        DequeueResult(CircularQueue queue, String value) {
            this.queue = queue;
            this.value = value;
        }
        DequeueResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'value': '%s'}", String.valueOf(queue), String.valueOf(value));
        }
    }


    static CircularQueue create_queue(int capacity) {
        String[] data = ((String[])(new String[]{}));
        int[] next = ((int[])(new int[]{}));
        int[] prev = ((int[])(new int[]{}));
        int i = 0;
        while (i < capacity) {
            data = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(data), java.util.stream.Stream.of("")).toArray(String[]::new)));
            next = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(next), java.util.stream.IntStream.of(Math.floorMod((i + 1), capacity))).toArray()));
            prev = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(prev), java.util.stream.IntStream.of(Math.floorMod((i - 1 + capacity), capacity))).toArray()));
            i = i + 1;
        }
        return new CircularQueue(data, next, prev, 0, 0);
    }

    static boolean is_empty(CircularQueue q) {
        return q.front == q.rear && (q.data[q.front].equals(""));
    }

    static void check_can_perform(CircularQueue q) {
        if (((Boolean)(is_empty(q)))) {
            throw new RuntimeException(String.valueOf("Empty Queue"));
        }
    }

    static void check_is_full(CircularQueue q) {
        if (q.next[q.rear] == q.front) {
            throw new RuntimeException(String.valueOf("Full Queue"));
        }
    }

    static String peek(CircularQueue q) {
        check_can_perform(q);
        return q.data[q.front];
    }

    static CircularQueue enqueue(CircularQueue q, String value) {
        check_is_full(q);
        if (!(Boolean)is_empty(q)) {
q.rear = q.next[q.rear];
        }
        String[] data_1 = ((String[])(q.data));
data_1[q.rear] = value;
q.data = data_1;
        return q;
    }

    static DequeueResult dequeue(CircularQueue q) {
        check_can_perform(q);
        String[] data_2 = ((String[])(q.data));
        String val = data_2[q.front];
data_2[q.front] = "";
q.data = data_2;
        if (q.front != q.rear) {
q.front = q.next[q.front];
        }
        return new DequeueResult(q, val);
    }

    static void main() {
        CircularQueue q = create_queue(3);
        System.out.println(_p(is_empty(q)));
        q = enqueue(q, "a");
        q = enqueue(q, "b");
        System.out.println(peek(q));
        DequeueResult res = dequeue(q);
        q = res.queue;
        System.out.println(res.value);
        res = dequeue(q);
        q = res.queue;
        System.out.println(res.value);
        System.out.println(_p(is_empty(q)));
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
