public class Main {

    static long[] make_list(long len, long value) {
        long[] arr = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < len) {
            arr = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(arr), java.util.stream.LongStream.of(value)).toArray()));
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return arr;
    }

    static long trapped_rainwater(long[] heights) {
        if ((long)(heights.length) == (long)(0)) {
            return 0;
        }
        long i_3 = 0L;
        while ((long)(i_3) < (long)(heights.length)) {
            if (heights[(int)((long)(i_3))] < (long)(0)) {
                throw new RuntimeException(String.valueOf("No height can be negative"));
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        long length_1 = (long)(heights.length);
        long[] left_max_1 = ((long[])(make_list(length_1, 0L)));
left_max_1[(int)((long)(0))] = heights[(int)((long)(0))];
        i_3 = (long)(1);
        while ((long)(i_3) < length_1) {
            if (heights[(int)((long)(i_3))] > (long)(left_max_1[(int)((long)((long)(i_3) - (long)(1)))])) {
left_max_1[(int)((long)(i_3))] = heights[(int)((long)(i_3))];
            } else {
left_max_1[(int)((long)(i_3))] = (long)(left_max_1[(int)((long)((long)(i_3) - (long)(1)))]);
            }
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        long[] right_max_1 = ((long[])(make_list(length_1, 0L)));
        long last_1 = length_1 - (long)(1);
right_max_1[(int)((long)(last_1))] = heights[(int)((long)(last_1))];
        i_3 = last_1 - (long)(1);
        while ((long)(i_3) >= (long)(0)) {
            if (heights[(int)((long)(i_3))] > (long)(right_max_1[(int)((long)((long)(i_3) + (long)(1)))])) {
right_max_1[(int)((long)(i_3))] = heights[(int)((long)(i_3))];
            } else {
right_max_1[(int)((long)(i_3))] = (long)(right_max_1[(int)((long)((long)(i_3) + (long)(1)))]);
            }
            i_3 = (long)((long)(i_3) - (long)(1));
        }
        long total_1 = 0L;
        i_3 = (long)(0);
        while ((long)(i_3) < length_1) {
            long left_1 = (long)(left_max_1[(int)((long)(i_3))]);
            long right_1 = (long)(right_max_1[(int)((long)(i_3))]);
            long smaller_1 = (long)((long)(left_1) < (long)(right_1) ? left_1 : right_1);
            total_1 = (long)(total_1) + ((long)(smaller_1) - heights[(int)((long)(i_3))]);
            i_3 = (long)((long)(i_3) + (long)(1));
        }
        return total_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(trapped_rainwater(((long[])(new long[]{0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1})))));
            System.out.println(_p(trapped_rainwater(((long[])(new long[]{7, 1, 5, 3, 6, 4})))));
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
}
