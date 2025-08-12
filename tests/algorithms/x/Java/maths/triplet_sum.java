public class Main {

    static long[] bubble_sort(long[] nums) {
        long[] arr = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(nums.length)) {
            arr = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(arr), java.util.stream.LongStream.of(_geti(nums, (int)((long)(i_1))))).toArray()));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        long n_1 = (long)(arr.length);
        long a_1 = 0L;
        while ((long)(a_1) < (long)(n_1)) {
            long b_1 = 0L;
            while ((long)(b_1) < (long)((long)((long)(n_1) - (long)(a_1)) - (long)(1))) {
                if (_geti(arr, (int)((long)(b_1))) > _geti(arr, (int)((long)((long)(b_1) + (long)(1))))) {
                    long tmp_1 = _geti(arr, (int)((long)(b_1)));
arr[(int)((long)(b_1))] = _geti(arr, (int)((long)((long)(b_1) + (long)(1))));
arr[(int)((long)((long)(b_1) + (long)(1)))] = tmp_1;
                }
                b_1 = (long)((long)(b_1) + (long)(1));
            }
            a_1 = (long)((long)(a_1) + (long)(1));
        }
        return arr;
    }

    static long[] sort3(long[] xs) {
        long[] arr_1 = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(xs.length)) {
            arr_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(arr_1), java.util.stream.LongStream.of(_geti(xs, (int)((long)(i_3))))).toArray()));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        long n_3 = (long)(arr_1.length);
        long a_3 = 0L;
        while ((long)(a_3) < (long)(n_3)) {
            long b_3 = 0L;
            while ((long)(b_3) < (long)((long)((long)(n_3) - (long)(a_3)) - (long)(1))) {
                if (_geti(arr_1, (int)((long)(b_3))) > _geti(arr_1, (int)((long)((long)(b_3) + (long)(1))))) {
                    long tmp_3 = _geti(arr_1, (int)((long)(b_3)));
arr_1[(int)((long)(b_3))] = _geti(arr_1, (int)((long)((long)(b_3) + (long)(1))));
arr_1[(int)((long)((long)(b_3) + (long)(1)))] = tmp_3;
                }
                b_3 = (long)((long)(b_3) + (long)(1));
            }
            a_3 = (long)((long)(a_3) + (long)(1));
        }
        return arr_1;
    }

    static long[] triplet_sum1(long[] arr, long target) {
        long i_4 = 0L;
        while ((long)(i_4) < (long)((long)(arr.length) - (long)(2))) {
            long j_1 = (long)((long)(i_4) + (long)(1));
            while ((long)(j_1) < (long)((long)(arr.length) - (long)(1))) {
                long k_1 = (long)((long)(j_1) + (long)(1));
                while ((long)(k_1) < (long)(arr.length)) {
                    if ((long)((long)(_geti(arr, (int)((long)(i_4))) + _geti(arr, (int)((long)(j_1)))) + _geti(arr, (int)((long)(k_1)))) == target) {
                        return sort3(((long[])(new long[]{_geti(arr, (int)((long)(i_4))), _geti(arr, (int)((long)(j_1))), _geti(arr, (int)((long)(k_1)))})));
                    }
                    k_1 = (long)((long)(k_1) + (long)(1));
                }
                j_1 = (long)((long)(j_1) + (long)(1));
            }
            i_4 = (long)((long)(i_4) + (long)(1));
        }
        return new long[]{0, 0, 0};
    }

    static long[] triplet_sum2(long[] arr, long target) {
        long[] sorted = ((long[])(bubble_sort(((long[])(arr)))));
        long n_5 = (long)(sorted.length);
        long i_6 = 0L;
        while ((long)(i_6) < (long)((long)(n_5) - (long)(2))) {
            long left_1 = (long)((long)(i_6) + (long)(1));
            long right_1 = (long)((long)(n_5) - (long)(1));
            while ((long)(left_1) < (long)(right_1)) {
                long s_1 = (long)((long)(_geti(sorted, (int)((long)(i_6))) + _geti(sorted, (int)((long)(left_1)))) + _geti(sorted, (int)((long)(right_1))));
                if ((long)(s_1) == target) {
                    return new long[]{_geti(sorted, (int)((long)(i_6))), _geti(sorted, (int)((long)(left_1))), _geti(sorted, (int)((long)(right_1)))};
                }
                if ((long)(s_1) < target) {
                    left_1 = (long)((long)(left_1) + (long)(1));
                } else {
                    right_1 = (long)((long)(right_1) - (long)(1));
                }
            }
            i_6 = (long)((long)(i_6) + (long)(1));
        }
        return new long[]{0, 0, 0};
    }

    static boolean list_equal(long[] a, long[] b) {
        if ((long)(a.length) != (long)(b.length)) {
            return false;
        }
        long i_8 = 0L;
        while ((long)(i_8) < (long)(a.length)) {
            if (_geti(a, (int)((long)(i_8))) != _geti(b, (int)((long)(i_8)))) {
                return false;
            }
            i_8 = (long)((long)(i_8) + (long)(1));
        }
        return true;
    }

    static void test_triplet_sum() {
        long[] arr1 = ((long[])(new long[]{13, 29, 7, 23, 5}));
        if (!(Boolean)list_equal(((long[])(triplet_sum1(((long[])(arr1)), 35L))), ((long[])(new long[]{5, 7, 23})))) {
            throw new RuntimeException(String.valueOf("ts1 case1 failed"));
        }
        if (!(Boolean)list_equal(((long[])(triplet_sum2(((long[])(arr1)), 35L))), ((long[])(new long[]{5, 7, 23})))) {
            throw new RuntimeException(String.valueOf("ts2 case1 failed"));
        }
        long[] arr2_1 = ((long[])(new long[]{37, 9, 19, 50, 44}));
        if (!(Boolean)list_equal(((long[])(triplet_sum1(((long[])(arr2_1)), 65L))), ((long[])(new long[]{9, 19, 37})))) {
            throw new RuntimeException(String.valueOf("ts1 case2 failed"));
        }
        if (!(Boolean)list_equal(((long[])(triplet_sum2(((long[])(arr2_1)), 65L))), ((long[])(new long[]{9, 19, 37})))) {
            throw new RuntimeException(String.valueOf("ts2 case2 failed"));
        }
        long[] arr3_1 = ((long[])(new long[]{6, 47, 27, 1, 15}));
        if (!(Boolean)list_equal(((long[])(triplet_sum1(((long[])(arr3_1)), 11L))), ((long[])(new long[]{0, 0, 0})))) {
            throw new RuntimeException(String.valueOf("ts1 case3 failed"));
        }
        if (!(Boolean)list_equal(((long[])(triplet_sum2(((long[])(arr3_1)), 11L))), ((long[])(new long[]{0, 0, 0})))) {
            throw new RuntimeException(String.valueOf("ts2 case3 failed"));
        }
    }

    static void main() {
        test_triplet_sum();
        long[] sample_1 = ((long[])(new long[]{13, 29, 7, 23, 5}));
        long[] res_1 = ((long[])(triplet_sum2(((long[])(sample_1)), 35L)));
        System.out.println(_p(_geti(res_1, ((Number)(0)).intValue())) + " " + _p(_geti(res_1, ((Number)(1)).intValue())) + " " + _p(_geti(res_1, ((Number)(2)).intValue())));
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
