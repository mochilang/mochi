public class Main {
    static long[][] matrix = ((long[][])(new long[][]{new long[]{0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0}, new long[]{0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0}, new long[]{0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0}, new long[]{0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0}, new long[]{0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0}}));

    static String encode(long row, long col) {
        return _p(row) + "," + _p(col);
    }

    static boolean is_safe(long row, long col, long rows, long cols) {
        return (long)(row) >= 0L && (long)(row) < (long)(rows) && (long)(col) >= 0L && (long)(col) < (long)(cols);
    }

    static boolean has(java.util.Map<String,Boolean> seen, String key) {
        return seen.containsKey(key);
    }

    static long depth_first_search(long row, long col, java.util.Map<String,Boolean> seen, long[][] mat) {
        long rows = (long)(mat.length);
        long cols_1 = (long)(mat[(int)(0L)].length);
        String key_1 = String.valueOf(encode((long)(row), (long)(col)));
        if (is_safe((long)(row), (long)(col), (long)(rows), (long)(cols_1)) && (!(Boolean)has(seen, key_1)) && (long)(mat[(int)((long)(row))][(int)((long)(col))]) == 1L) {
seen.put(key_1, true);
            return (long)((long)((long)(1L + (long)(depth_first_search((long)((long)(row) + 1L), (long)(col), seen, ((long[][])(mat))))) + (long)(depth_first_search((long)((long)(row) - 1L), (long)(col), seen, ((long[][])(mat))))) + (long)(depth_first_search((long)(row), (long)((long)(col) + 1L), seen, ((long[][])(mat))))) + (long)(depth_first_search((long)(row), (long)((long)(col) - 1L), seen, ((long[][])(mat))));
        } else {
            return 0;
        }
    }

    static long find_max_area(long[][] mat) {
        java.util.Map<String,Boolean> seen = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        long rows_2 = (long)(mat.length);
        long max_area_1 = 0L;
        long r_1 = 0L;
        while ((long)(r_1) < (long)(rows_2)) {
            long[] line_1 = ((long[])(mat[(int)((long)(r_1))]));
            long cols_3 = (long)(line_1.length);
            long c_1 = 0L;
            while ((long)(c_1) < (long)(cols_3)) {
                if ((long)(line_1[(int)((long)(c_1))]) == 1L) {
                    String key_3 = String.valueOf(encode((long)(r_1), (long)(c_1)));
                    if (!(seen.containsKey(key_3))) {
                        long area_1 = (long)(depth_first_search((long)(r_1), (long)(c_1), seen, ((long[][])(mat))));
                        if ((long)(area_1) > (long)(max_area_1)) {
                            max_area_1 = (long)(area_1);
                        }
                    }
                }
                c_1 = (long)((long)(c_1) + 1L);
            }
            r_1 = (long)((long)(r_1) + 1L);
        }
        return max_area_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(find_max_area(((long[][])(matrix))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{\"duration_us\": " + _benchDuration + ", \"memory_bytes\": " + _benchMemory + ", \"name\": \"main\"}");
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
