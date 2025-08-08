public class Main {
    static class Deque {
        int[] data;
        Deque(int[] data) {
            this.data = data;
        }
        Deque() {}
        @Override public String toString() {
            return String.format("{'data': %s}", String.valueOf(data));
        }
    }

    static class PopResult {
        Deque deque;
        int value;
        PopResult(Deque deque, int value) {
            this.deque = deque;
            this.value = value;
        }
        PopResult() {}
        @Override public String toString() {
            return String.format("{'deque': %s, 'value': %s}", String.valueOf(deque), String.valueOf(value));
        }
    }


    static Deque empty_deque() {
        return new Deque(new int[]{});
    }

    static Deque push_back(Deque dq, int value) {
        return new Deque(java.util.stream.IntStream.concat(java.util.Arrays.stream(dq.data), java.util.stream.IntStream.of(value)).toArray());
    }

    static Deque push_front(Deque dq, int value) {
        int[] res = ((int[])(new int[]{value}));
        int i = 0;
        while (i < dq.data.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(dq.data[i])).toArray()));
            i = i + 1;
        }
        return new Deque(res);
    }

    static Deque extend_back(Deque dq, int[] values) {
        int[] res_1 = ((int[])(dq.data));
        int i_1 = 0;
        while (i_1 < values.length) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(values[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        return new Deque(res_1);
    }

    static Deque extend_front(Deque dq, int[] values) {
        int[] res_2 = ((int[])(new int[]{}));
        int i_2 = values.length - 1;
        while (i_2 >= 0) {
            res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(values[i_2])).toArray()));
            i_2 = i_2 - 1;
        }
        int j = 0;
        while (j < dq.data.length) {
            res_2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_2), java.util.stream.IntStream.of(dq.data[j])).toArray()));
            j = j + 1;
        }
        return new Deque(res_2);
    }

    static PopResult pop_back(Deque dq) {
        if (dq.data.length == 0) {
            throw new RuntimeException(String.valueOf("pop from empty deque"));
        }
        int[] res_3 = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < dq.data.length - 1) {
            res_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_3), java.util.stream.IntStream.of(dq.data[i_3])).toArray()));
            i_3 = i_3 + 1;
        }
        return new PopResult(new Deque(res_3), dq.data[dq.data.length - 1]);
    }

    static PopResult pop_front(Deque dq) {
        if (dq.data.length == 0) {
            throw new RuntimeException(String.valueOf("popleft from empty deque"));
        }
        int[] res_4 = ((int[])(new int[]{}));
        int i_4 = 1;
        while (i_4 < dq.data.length) {
            res_4 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_4), java.util.stream.IntStream.of(dq.data[i_4])).toArray()));
            i_4 = i_4 + 1;
        }
        return new PopResult(new Deque(res_4), dq.data[0]);
    }

    static boolean is_empty(Deque dq) {
        return dq.data.length == 0;
    }

    static int length(Deque dq) {
        return dq.data.length;
    }

    static String to_string(Deque dq) {
        if (dq.data.length == 0) {
            return "[]";
        }
        String s = "[" + _p(_geti(dq.data, 0));
        int i_5 = 1;
        while (i_5 < dq.data.length) {
            s = s + ", " + _p(_geti(dq.data, i_5));
            i_5 = i_5 + 1;
        }
        return s + "]";
    }

    static void main() {
        Deque dq = empty_deque();
        dq = push_back(dq, 2);
        dq = push_front(dq, 1);
        dq = extend_back(dq, ((int[])(new int[]{3, 4})));
        dq = extend_front(dq, ((int[])(new int[]{0})));
        System.out.println(to_string(dq));
        PopResult r = pop_back(dq);
        dq = r.deque;
        System.out.println(r.value);
        r = pop_front(dq);
        dq = r.deque;
        System.out.println(r.value);
        System.out.println(to_string(dq));
        System.out.println(is_empty(empty_deque()));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
