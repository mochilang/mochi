public class Main {

    static double abs_float(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double average_absolute_deviation(int[] nums) {
        if (nums.length == 0) {
            throw new RuntimeException(String.valueOf("List is empty"));
        }
        int sum = 0;
        for (int x : nums) {
            sum = sum + x;
        }
        double n = ((Number)(nums.length)).doubleValue();
        double mean = (((Number)(sum)).doubleValue()) / n;
        double dev_sum = 0.0;
        for (int x : nums) {
            dev_sum = dev_sum + abs_float((((Number)(x)).doubleValue()) - mean);
        }
        return dev_sum / n;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(average_absolute_deviation(((int[])(new int[]{0})))));
            System.out.println(_p(average_absolute_deviation(((int[])(new int[]{4, 1, 3, 2})))));
            System.out.println(_p(average_absolute_deviation(((int[])(new int[]{2, 70, 6, 50, 20, 8, 4, 0})))));
            System.out.println(_p(average_absolute_deviation(((int[])(new int[]{-20, 0, 30, 15})))));
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
