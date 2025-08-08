public class Main {
    static class Queue {
        int[] stack1;
        int[] stack2;
        Queue(int[] stack1, int[] stack2) {
            this.stack1 = stack1;
            this.stack2 = stack2;
        }
        Queue() {}
        @Override public String toString() {
            return String.format("{'stack1': %s, 'stack2': %s}", String.valueOf(stack1), String.valueOf(stack2));
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
    static GetResult r1;
    static GetResult r2;
    static GetResult r3;
    static GetResult r4;

    static Queue new_queue(int[] items) {
        return new Queue(items, new int[]{});
    }

    static int len_queue(Queue q) {
        return q.stack1.length + q.stack2.length;
    }

    static String str_queue(Queue q) {
        int[] items = ((int[])(new int[]{}));
        int i = q.stack2.length - 1;
        while (i >= 0) {
            items = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(items), java.util.stream.IntStream.of(q.stack2[i])).toArray()));
            i = i - 1;
        }
        int j = 0;
        while (j < q.stack1.length) {
            items = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(items), java.util.stream.IntStream.of(q.stack1[j])).toArray()));
            j = j + 1;
        }
        String s = "Queue((";
        int k = 0;
        while (k < items.length) {
            s = s + _p(_geti(items, k));
            if (k < items.length - 1) {
                s = s + ", ";
            }
            k = k + 1;
        }
        s = s + "))";
        return s;
    }

    static Queue put(Queue q, int item) {
        int[] s1 = ((int[])(q.stack1));
        s1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(s1), java.util.stream.IntStream.of(item)).toArray()));
        return new Queue(s1, q.stack2);
    }

    static GetResult get(Queue q) {
        int[] s1_1 = ((int[])(q.stack1));
        int[] s2 = ((int[])(q.stack2));
        if (s2.length == 0) {
            while (s1_1.length > 0) {
                int idx = s1_1.length - 1;
                int v = s1_1[idx];
                int[] new_s1 = ((int[])(new int[]{}));
                int i_1 = 0;
                while (i_1 < idx) {
                    new_s1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_s1), java.util.stream.IntStream.of(s1_1[i_1])).toArray()));
                    i_1 = i_1 + 1;
                }
                s1_1 = ((int[])(new_s1));
                s2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(s2), java.util.stream.IntStream.of(v)).toArray()));
            }
        }
        if (s2.length == 0) {
            throw new RuntimeException(String.valueOf("Queue is empty"));
        }
        int idx2 = s2.length - 1;
        int value = s2[idx2];
        int[] new_s2 = ((int[])(new int[]{}));
        int j_1 = 0;
        while (j_1 < idx2) {
            new_s2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_s2), java.util.stream.IntStream.of(s2[j_1])).toArray()));
            j_1 = j_1 + 1;
        }
        s2 = ((int[])(new_s2));
        return new GetResult(new Queue(s1_1, s2), value);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            q = new_queue(((int[])(new int[]{10, 20, 30})));
            r1 = get(q);
            q = r1.queue;
            System.out.println(r1.value);
            q = put(q, 40);
            r2 = get(q);
            q = r2.queue;
            System.out.println(r2.value);
            r3 = get(q);
            q = r3.queue;
            System.out.println(r3.value);
            System.out.println(len_queue(q));
            r4 = get(q);
            q = r4.queue;
            System.out.println(r4.value);
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
