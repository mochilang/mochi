public class Main {
    static class Queue {
        int[] stack;
        int length;
        Queue(int[] stack, int length) {
            this.stack = stack;
            this.length = length;
        }
        Queue() {}
        @Override public String toString() {
            return String.format("{'stack': %s, 'length': %s}", String.valueOf(stack), String.valueOf(length));
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

    static class FrontResult {
        Queue queue;
        int value;
        FrontResult(Queue queue, int value) {
            this.queue = queue;
            this.value = value;
        }
        FrontResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'value': %s}", String.valueOf(queue), String.valueOf(value));
        }
    }


    static Queue empty_queue() {
        return new Queue(new int[]{}, 0);
    }

    static Queue put(Queue q, int item) {
        int[] s = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q.stack), java.util.stream.IntStream.of(item)).toArray()));
        return new Queue(s, q.length + 1);
    }

    static int[] drop_first(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int i = 1;
        while (i < xs.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static int[] drop_last(int[] xs) {
        int[] res_1 = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < xs.length - 1) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static Queue rotate(Queue q, int rotation) {
        int[] s_1 = ((int[])(q.stack));
        int i_2 = 0;
        while (i_2 < rotation && s_1.length > 0) {
            int temp = s_1[0];
            s_1 = ((int[])(drop_first(((int[])(s_1)))));
            s_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(s_1), java.util.stream.IntStream.of(temp)).toArray()));
            i_2 = i_2 + 1;
        }
        return new Queue(s_1, q.length);
    }

    static GetResult get(Queue q) {
        if (q.length == 0) {
            throw new RuntimeException(String.valueOf("queue empty"));
        }
        Queue q1 = rotate(q, 1);
        int v = q1.stack[q1.length - 1];
        int[] s_2 = ((int[])(drop_last(((int[])(q1.stack)))));
        Queue q2 = new Queue(s_2, q1.length);
        q2 = rotate(q2, q2.length - 1);
        q2 = new Queue(q2.stack, q2.length - 1);
        return new GetResult(q2, v);
    }

    static FrontResult front(Queue q) {
        GetResult r = get(q);
        Queue q2_1 = put(r.queue, r.value);
        q2_1 = rotate(q2_1, q2_1.length - 1);
        return new FrontResult(q2_1, r.value);
    }

    static int size(Queue q) {
        return q.length;
    }

    static String to_string(Queue q) {
        String s_3 = "<";
        if (q.length > 0) {
            s_3 = s_3 + _p(_geti(q.stack, 0));
            int i_3 = 1;
            while (i_3 < q.length) {
                s_3 = s_3 + ", " + _p(_geti(q.stack, i_3));
                i_3 = i_3 + 1;
            }
        }
        s_3 = s_3 + ">";
        return s_3;
    }

    static void main() {
        Queue q = empty_queue();
        q = put(q, 1);
        q = put(q, 2);
        q = put(q, 3);
        System.out.println(to_string(q));
        GetResult g = get(q);
        q = g.queue;
        System.out.println(g.value);
        System.out.println(to_string(q));
        FrontResult f = front(q);
        q = f.queue;
        System.out.println(f.value);
        System.out.println(to_string(q));
        System.out.println(size(q));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
