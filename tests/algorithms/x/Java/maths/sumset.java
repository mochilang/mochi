public class Main {

    static boolean contains(int[] xs, int value) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == value) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static int[] sumset(int[] set_a, int[] set_b) {
        int[] result = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < set_a.length) {
            int j = 0;
            while (j < set_b.length) {
                int s = set_a[i_1] + set_b[j];
                if (!(Boolean)contains(((int[])(result)), s)) {
                    result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(s)).toArray()));
                }
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void main() {
        int[] set_a = ((int[])(new int[]{1, 2, 3}));
        int[] set_b = ((int[])(new int[]{4, 5, 6}));
        System.out.println(_p(sumset(((int[])(set_a)), ((int[])(set_b)))));
        int[] set_c = ((int[])(new int[]{4, 5, 6, 7}));
        System.out.println(_p(sumset(((int[])(set_a)), ((int[])(set_c)))));
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
}
