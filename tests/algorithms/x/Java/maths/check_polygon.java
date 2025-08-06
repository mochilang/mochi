public class Main {
    static double[] nums = new double[0];
    static boolean _v;

    static boolean check_polygon(double[] nums) {
        if (nums.length < 2) {
            throw new RuntimeException(String.valueOf("Monogons and Digons are not polygons in the Euclidean space"));
        }
        int i = 0;
        while (i < nums.length) {
            if (nums[i] <= 0.0) {
                throw new RuntimeException(String.valueOf("All values must be greater than 0"));
            }
            i = i + 1;
        }
        double total = 0.0;
        double max_side = 0.0;
        i = 0;
        while (i < nums.length) {
            double v = nums[i];
            total = total + v;
            if (v > max_side) {
                max_side = v;
            }
            i = i + 1;
        }
        return max_side < (total - max_side);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(check_polygon(((double[])(new double[]{6.0, 10.0, 5.0})))));
            System.out.println(_p(check_polygon(((double[])(new double[]{3.0, 7.0, 13.0, 2.0})))));
            System.out.println(_p(check_polygon(((double[])(new double[]{1.0, 4.3, 5.2, 12.2})))));
            nums = ((double[])(new double[]{3.0, 7.0, 13.0, 2.0}));
            _v = check_polygon(((double[])(nums)));
            System.out.println(_p(nums));
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
