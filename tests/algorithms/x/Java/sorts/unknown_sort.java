public class Main {

    static long list_min(long[] xs) {
        long i = 1;
        long m = xs[(int)(0)];
        while (i < xs.length) {
            if (xs[(int)(i)] < m) {
                m = xs[(int)(i)];
            }
            i = i + 1;
        }
        return m;
    }

    static long list_max(long[] xs) {
        long i_1 = 1;
        long m_1 = xs[(int)(0)];
        while (i_1 < xs.length) {
            if (xs[(int)(i_1)] > m_1) {
                m_1 = xs[(int)(i_1)];
            }
            i_1 = i_1 + 1;
        }
        return m_1;
    }

    static long[] remove_once(long[] xs, long value) {
        long[] res = ((long[])(new long[]{}));
        boolean removed = false;
        long i_2 = 0;
        while (i_2 < xs.length) {
            if (!(Boolean)removed && xs[(int)(i_2)] == value) {
                removed = true;
            } else {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(xs[(int)(i_2)])).toArray()));
            }
            i_2 = i_2 + 1;
        }
        return res;
    }

    static long[] reverse_list(long[] xs) {
        long[] res_1 = ((long[])(new long[]{}));
        long i_3 = xs.length - 1;
        while (i_3 >= 0) {
            res_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res_1), java.util.stream.LongStream.of(xs[(int)(i_3)])).toArray()));
            i_3 = i_3 - 1;
        }
        return res_1;
    }

    static long[] merge_sort(long[] collection) {
        long[] start = ((long[])(new long[]{}));
        long[] end = ((long[])(new long[]{}));
        long[] coll = ((long[])(collection));
        while (coll.length > 1) {
            long mn = list_min(((long[])(coll)));
            long mx = list_max(((long[])(coll)));
            start = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(start), java.util.stream.LongStream.of(mn)).toArray()));
            end = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(end), java.util.stream.LongStream.of(mx)).toArray()));
            coll = ((long[])(remove_once(((long[])(coll)), mn)));
            coll = ((long[])(remove_once(((long[])(coll)), mx)));
        }
        end = ((long[])(reverse_list(((long[])(end)))));
        return java.util.stream.LongStream.concat(java.util.Arrays.stream(java.util.stream.LongStream.concat(java.util.Arrays.stream(start), java.util.Arrays.stream(coll)).toArray()), java.util.Arrays.stream(end)).toArray();
    }

    static void test_merge_sort() {
        if (!java.util.Arrays.equals(_toObjectArray(merge_sort(((long[])(new long[]{0, 5, 3, 2, 2})))), _toObjectArray(new long[]{0, 2, 2, 3, 5}))) {
            throw new RuntimeException(String.valueOf("case1 failed"));
        }
        if (!java.util.Arrays.equals(_toObjectArray(merge_sort(((long[])(new long[]{})))), _toObjectArray(new Object[]{}))) {
            throw new RuntimeException(String.valueOf("case2 failed"));
        }
        if (!java.util.Arrays.equals(_toObjectArray(merge_sort(((long[])(new long[]{-2, -5, -45})))), _toObjectArray(new long[]{-45, -5, -2}))) {
            throw new RuntimeException(String.valueOf("case3 failed"));
        }
    }

    static void main() {
        test_merge_sort();
        System.out.println(_p(merge_sort(((long[])(new long[]{0, 5, 3, 2, 2})))));
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

    static Object[] _toObjectArray(Object v) {
        if (v instanceof Object[]) return (Object[]) v;
        if (v instanceof int[]) return java.util.Arrays.stream((int[]) v).boxed().toArray();
        if (v instanceof double[]) return java.util.Arrays.stream((double[]) v).boxed().toArray();
        if (v instanceof long[]) return java.util.Arrays.stream((long[]) v).boxed().toArray();
        if (v instanceof boolean[]) { boolean[] a = (boolean[]) v; Object[] out = new Object[a.length]; for (int i = 0; i < a.length; i++) out[i] = a[i]; return out; }
        return (Object[]) v;
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
