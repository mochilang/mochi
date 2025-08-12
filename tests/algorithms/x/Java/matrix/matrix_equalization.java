public class Main {

    static long[] unique(long[] nums) {
        long[] res = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(nums.length)) {
            long v_1 = _geti(nums, (int)((long)(i_1)));
            boolean found_1 = false;
            long j_1 = 0L;
            while ((long)(j_1) < (long)(res.length)) {
                if (_geti(res, (int)((long)(j_1))) == v_1) {
                    found_1 = true;
                    break;
                }
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            if (!found_1) {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(v_1)).toArray()));
            }
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return res;
    }

    static long array_equalization(long[] vector, long step_size) {
        if (step_size <= (long)(0)) {
            throw new RuntimeException(String.valueOf("Step size must be positive and non-zero."));
        }
        long[] elems_1 = ((long[])(unique(((long[])(vector)))));
        long min_updates_1 = (long)(vector.length);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(elems_1.length)) {
            long target_1 = _geti(elems_1, (int)((long)(i_3)));
            long idx_1 = 0L;
            long updates_1 = 0L;
            while ((long)(idx_1) < (long)(vector.length)) {
                if (_geti(vector, (int)((long)(idx_1))) != target_1) {
                    updates_1 = (long)((long)(updates_1) + (long)(1));
                    idx_1 = (long)((long)(idx_1) + step_size);
                } else {
                    idx_1 = (long)((long)(idx_1) + (long)(1));
                }
            }
            if ((long)(updates_1) < (long)(min_updates_1)) {
                min_updates_1 = (long)(updates_1);
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return min_updates_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(array_equalization(((long[])(new long[]{1, 1, 6, 2, 4, 6, 5, 1, 7, 2, 2, 1, 7, 2, 2})), 4L)));
            System.out.println(_p(array_equalization(((long[])(new long[]{22, 81, 88, 71, 22, 81, 632, 81, 81, 22, 92})), 2L)));
            System.out.println(_p(array_equalization(((long[])(new long[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0})), 5L)));
            System.out.println(_p(array_equalization(((long[])(new long[]{22, 22, 22, 33, 33, 33})), 2L)));
            System.out.println(_p(array_equalization(((long[])(new long[]{1, 2, 3})), 2147483647L)));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static long _geti(long[] a, int i) {
        if (a == null) return 0L;
        if (i < 0) i += a.length;
        if (i < 0 || i >= a.length) return 0L;
        return a[i];
    }
}
