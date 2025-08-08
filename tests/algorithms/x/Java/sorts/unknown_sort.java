public class Main {

    static int list_min(int[] xs) {
        int i = 1;
        int m = xs[0];
        while (i < xs.length) {
            if (xs[i] < m) {
                m = xs[i];
            }
            i = i + 1;
        }
        return m;
    }

    static int list_max(int[] xs) {
        int i_1 = 1;
        int m_1 = xs[0];
        while (i_1 < xs.length) {
            if (xs[i_1] > m_1) {
                m_1 = xs[i_1];
            }
            i_1 = i_1 + 1;
        }
        return m_1;
    }

    static int[] remove_once(int[] xs, int value) {
        int[] res = ((int[])(new int[]{}));
        boolean removed = false;
        int i_2 = 0;
        while (i_2 < xs.length) {
            if (!(Boolean)removed && xs[i_2] == value) {
                removed = true;
            } else {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i_2])).toArray()));
            }
            i_2 = i_2 + 1;
        }
        return res;
    }

    static int[] reverse_list(int[] xs) {
        int[] res_1 = ((int[])(new int[]{}));
        int i_3 = xs.length - 1;
        while (i_3 >= 0) {
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(xs[i_3])).toArray()));
            i_3 = i_3 - 1;
        }
        return res_1;
    }

    static int[] merge_sort(int[] collection) {
        int[] start = ((int[])(new int[]{}));
        int[] end = ((int[])(new int[]{}));
        int[] coll = ((int[])(collection));
        while (coll.length > 1) {
            int mn = list_min(((int[])(coll)));
            int mx = list_max(((int[])(coll)));
            start = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(start), java.util.stream.IntStream.of(mn)).toArray()));
            end = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(end), java.util.stream.IntStream.of(mx)).toArray()));
            coll = ((int[])(remove_once(((int[])(coll)), mn)));
            coll = ((int[])(remove_once(((int[])(coll)), mx)));
        }
        end = ((int[])(reverse_list(((int[])(end)))));
        return java.util.stream.IntStream.concat(java.util.Arrays.stream(java.util.stream.IntStream.concat(java.util.Arrays.stream(start), java.util.Arrays.stream(coll)).toArray()), java.util.Arrays.stream(end)).toArray();
    }

    static void test_merge_sort() {
        if (merge_sort(((int[])(new int[]{0, 5, 3, 2, 2}))) != new int[]{0, 2, 2, 3, 5}) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (merge_sort(((int[])(new int[]{}))) != ((Number)(new Object[]{})).intValue()) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (merge_sort(((int[])(new int[]{-2, -5, -45}))) != new int[]{-45, -5, -2}) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
    }

    static void main() {
        test_merge_sort();
        System.out.println(_p(merge_sort(((int[])(new int[]{0, 5, 3, 2, 2})))));
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
