public class Main {

    static int[] remove_at(int[] xs, int idx) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length) {
            if (i != idx) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            }
            i = i + 1;
        }
        return res;
    }

    static int[] kth_permutation(int k, int n) {
        if (n <= 0) {
            throw new RuntimeException(String.valueOf("n must be positive"));
        }
        int[] factorials = ((int[])(new int[]{1}));
        int i_1 = 2;
        while (i_1 < n) {
            factorials = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(factorials), java.util.stream.IntStream.of(factorials[factorials.length - 1] * i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        int total = factorials[factorials.length - 1] * n;
        if ((k < 0) || (k >= total)) {
            throw new RuntimeException(String.valueOf("k out of bounds"));
        }
        int[] elements = ((int[])(new int[]{}));
        int e = 0;
        while (e < n) {
            elements = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(elements), java.util.stream.IntStream.of(e)).toArray()));
            e = e + 1;
        }
        int[] permutation = ((int[])(new int[]{}));
        int idx = factorials.length - 1;
        while (idx >= 0) {
            int factorial = factorials[idx];
            int number = k / factorial;
            k = Math.floorMod(k, factorial);
            permutation = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(permutation), java.util.stream.IntStream.of(elements[number])).toArray()));
            elements = ((int[])(remove_at(((int[])(elements)), number)));
            idx = idx - 1;
        }
        permutation = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(permutation), java.util.stream.IntStream.of(elements[0])).toArray()));
        return permutation;
    }

    static boolean list_equal(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        int i_2 = 0;
        while (i_2 < a.length) {
            if (a[i_2] != b[i_2]) {
                return false;
            }
            i_2 = i_2 + 1;
        }
        return true;
    }

    static String list_to_string(int[] xs) {
        if (xs.length == 0) {
            return "[]";
        }
        String s = "[" + _p(_geti(xs, 0));
        int i_3 = 1;
        while (i_3 < xs.length) {
            s = s + ", " + _p(_geti(xs, i_3));
            i_3 = i_3 + 1;
        }
        s = s + "]";
        return s;
    }

    static void test_kth_permutation() {
        int[] expected1 = ((int[])(new int[]{0, 1, 2, 3, 4}));
        int[] res1 = ((int[])(kth_permutation(0, 5)));
        if (!(Boolean)list_equal(((int[])(res1)), ((int[])(expected1)))) {
            throw new RuntimeException(String.valueOf("test case 1 failed"));
        }
        int[] expected2 = ((int[])(new int[]{1, 3, 0, 2}));
        int[] res2 = ((int[])(kth_permutation(10, 4)));
        if (!(Boolean)list_equal(((int[])(res2)), ((int[])(expected2)))) {
            throw new RuntimeException(String.valueOf("test case 2 failed"));
        }
    }

    static void main() {
        test_kth_permutation();
        int[] res_1 = ((int[])(kth_permutation(10, 4)));
        System.out.println(list_to_string(((int[])(res_1))));
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
