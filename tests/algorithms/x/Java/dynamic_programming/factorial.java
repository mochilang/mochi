public class Main {
    static int[] memo = new int[0];
    static int[] results = new int[0];

    static int factorial(int num) {
        if (num < 0) {
            System.out.println("Number should not be negative.");
            return 0;
        }
        int[] m = ((int[])(memo));
        int i = m.length;
        while (i <= num) {
            m = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(m), java.util.stream.IntStream.of(i * m[i - 1])).toArray()));
            i = i + 1;
        }
        memo = ((int[])(m));
        return m[num];
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            memo = ((int[])(new int[]{1, 1}));
            System.out.println(_p(factorial(7)));
            factorial(-1);
            results = ((int[])(new int[]{}));
            for (int i = 0; i < 10; i++) {
                results = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(results), java.util.stream.IntStream.of(factorial(i))).toArray()));
            }
            System.out.println(_p(results));
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
