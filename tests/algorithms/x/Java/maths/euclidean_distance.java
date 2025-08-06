public class Main {

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double euclidean_distance(double[] v1, double[] v2) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < v1.length) {
            double diff = v1[i_1] - v2[i_1];
            sum = sum + diff * diff;
            i_1 = i_1 + 1;
        }
        return sqrtApprox(sum);
    }

    static double euclidean_distance_no_np(double[] v1, double[] v2) {
        return euclidean_distance(((double[])(v1)), ((double[])(v2)));
    }

    static void main() {
        System.out.println(_p(euclidean_distance(((double[])(new double[]{0.0, 0.0})), ((double[])(new double[]{2.0, 2.0})))));
        System.out.println(_p(euclidean_distance(((double[])(new double[]{0.0, 0.0, 0.0})), ((double[])(new double[]{2.0, 2.0, 2.0})))));
        System.out.println(_p(euclidean_distance(((double[])(new double[]{1.0, 2.0, 3.0, 4.0})), ((double[])(new double[]{5.0, 6.0, 7.0, 8.0})))));
        System.out.println(_p(euclidean_distance_no_np(((double[])(new double[]{1.0, 2.0, 3.0, 4.0})), ((double[])(new double[]{5.0, 6.0, 7.0, 8.0})))));
        System.out.println(_p(euclidean_distance_no_np(((double[])(new double[]{0.0, 0.0})), ((double[])(new double[]{2.0, 2.0})))));
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
