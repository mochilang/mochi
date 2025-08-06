public class Main {
    static class Queue {
        int[] entries;
        Queue(int[] entries) {
            this.entries = entries;
        }
        Queue() {}
        @Override public String toString() {
            return String.format("{'entries': %s}", String.valueOf(entries));
        }
    }

    static class GetResult {
        Queue queue;
        int value;
        GetResult(Queue queue, int value) {
            this.queue = queue;
            this.value = value;
        }
        GetResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'value': %s}", String.valueOf(queue), String.valueOf(value));
        }
    }

    static Queue q = null;
    static GetResult res;
    static int front;

    static Queue new_queue(int[] items) {
        return new Queue(items);
    }

    static int len_queue(Queue q) {
        return q.entries.length;
    }

    static String str_queue(Queue q) {
        String s = "Queue((";
        int i = 0;
        while (i < q.entries.length) {
            s = s + _p(_geti(q.entries, i));
            if (i < q.entries.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        s = s + "))";
        return s;
    }

    static Queue put(Queue q, int item) {
        int[] e = ((int[])(q.entries));
        e = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(e), java.util.stream.IntStream.of(item)).toArray()));
        return new Queue(e);
    }

    static GetResult get(Queue q) {
        if (q.entries.length == 0) {
            throw new RuntimeException(String.valueOf("Queue is empty"));
        }
        int value = q.entries[0];
        int[] new_entries = ((int[])(new int[]{}));
        int i_1 = 1;
        while (i_1 < q.entries.length) {
            new_entries = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_entries), java.util.stream.IntStream.of(q.entries[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return new GetResult(new Queue(new_entries), value);
    }

    static Queue rotate(Queue q, int rotation) {
        int[] e_1 = ((int[])(q.entries));
        int r = 0;
        while (r < rotation) {
            if (e_1.length > 0) {
                int first = e_1[0];
                int[] rest = ((int[])(new int[]{}));
                int i_2 = 1;
                while (i_2 < e_1.length) {
                    rest = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(rest), java.util.stream.IntStream.of(e_1[i_2])).toArray()));
                    i_2 = i_2 + 1;
                }
                rest = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(rest), java.util.stream.IntStream.of(first)).toArray()));
                e_1 = ((int[])(rest));
            }
            r = r + 1;
        }
        return new Queue(e_1);
    }

    static int get_front(Queue q) {
        return q.entries[0];
    }
    public static void main(String[] args) {
        q = new_queue(((int[])(new int[]{})));
        System.out.println(len_queue(q));
        q = put(q, 10);
        q = put(q, 20);
        q = put(q, 30);
        q = put(q, 40);
        System.out.println(str_queue(q));
        res = get(q);
        q = res.queue;
        System.out.println(res.value);
        System.out.println(str_queue(q));
        q = rotate(q, 2);
        System.out.println(str_queue(q));
        front = get_front(q);
        System.out.println(front);
        System.out.println(str_queue(q));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
