public class Main {

    static double abs_val(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static void validate_point(double[] p) {
        if (p.length == 0) {
            throw new RuntimeException(String.valueOf("Missing an input"));
        }
    }

    static double manhattan_distance(double[] a, double[] b) {
        validate_point(((double[])(a)));
        validate_point(((double[])(b)));
        if (a.length != b.length) {
            throw new RuntimeException(String.valueOf("Both points must be in the same n-dimensional space"));
        }
        double total_1 = 0.0;
        long i_1 = 0;
        while (i_1 < a.length) {
            total_1 = total_1 + abs_val(a[(int)(i_1)] - b[(int)(i_1)]);
            i_1 = i_1 + 1;
        }
        return total_1;
    }

    static double manhattan_distance_one_liner(double[] a, double[] b) {
        return manhattan_distance(((double[])(a)), ((double[])(b)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(manhattan_distance(((double[])(new double[]{1.0, 1.0})), ((double[])(new double[]{2.0, 2.0})))));
            System.out.println(_p(manhattan_distance(((double[])(new double[]{1.5, 1.5})), ((double[])(new double[]{2.0, 2.0})))));
            System.out.println(_p(manhattan_distance_one_liner(((double[])(new double[]{1.5, 1.5})), ((double[])(new double[]{2.5, 2.0})))));
            System.out.println(_p(manhattan_distance_one_liner(((double[])(new double[]{-3.0, -3.0, -3.0})), ((double[])(new double[]{0.0, 0.0, 0.0})))));
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
