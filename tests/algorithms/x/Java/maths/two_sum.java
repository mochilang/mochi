public class Main {

    static long[] two_sum(long[] nums, long target) {
        java.util.Map<Long,Long> chk_map = ((java.util.Map<Long,Long>)(new java.util.LinkedHashMap<Long, Long>()));
        long idx_1 = 0L;
        while ((long)(idx_1) < (long)(nums.length)) {
            long val_1 = _geti(nums, (int)((long)(idx_1)));
            long compl_1 = (long)(target - val_1);
            if (chk_map.containsKey(compl_1)) {
                return new long[]{(long)(((long)(chk_map).getOrDefault(compl_1, 0L))) - (long)(1), idx_1};
            }
chk_map.put(val_1, (long)((long)(idx_1) + (long)(1)));
            idx_1 = (long)((long)(idx_1) + (long)(1));
        }
        return new long[]{};
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(two_sum(((long[])(new long[]{2, 7, 11, 15})), 9L)));
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
