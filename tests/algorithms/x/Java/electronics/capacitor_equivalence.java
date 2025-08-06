public class Main {

    static double capacitor_parallel(double[] capacitors) {
        double sum_c = 0.0;
        int i = 0;
        while (i < capacitors.length) {
            double c = capacitors[i];
            if (c < 0.0) {
                throw new RuntimeException(String.valueOf("Capacitor at index " + _p(i) + " has a negative value!"));
                return 0.0;
            }
            sum_c = sum_c + c;
            i = i + 1;
        }
        return sum_c;
    }

    static double capacitor_series(double[] capacitors) {
        double first_sum = 0.0;
        int i_1 = 0;
        while (i_1 < capacitors.length) {
            double c_1 = capacitors[i_1];
            if (c_1 <= 0.0) {
                throw new RuntimeException(String.valueOf("Capacitor at index " + _p(i_1) + " has a negative or zero value!"));
                return 0.0;
            }
            first_sum = first_sum + 1.0 / c_1;
            i_1 = i_1 + 1;
        }
        return 1.0 / first_sum;
    }

    static void main() {
        double parallel = capacitor_parallel(((double[])(new double[]{5.71389, 12.0, 3.0})));
        double series = capacitor_series(((double[])(new double[]{5.71389, 12.0, 3.0})));
        System.out.println(_p(parallel));
        System.out.println(_p(series));
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
