public class Main {

    static double capacitor_parallel(double[] capacitors) {
        double sum_c = 0.0;
        long i = 0;
        while (i < capacitors.length) {
            double c = capacitors[(int)(i)];
            if (c < 0.0) {
                throw new RuntimeException(String.valueOf("Capacitor at index " + _p(i) + " has a negative value!"));
            }
            sum_c = sum_c + c;
            i = i + 1;
        }
        return sum_c;
    }

    static double capacitor_series(double[] capacitors) {
        double first_sum = 0.0;
        long i_1 = 0;
        while (i_1 < capacitors.length) {
            double c_1 = capacitors[(int)(i_1)];
            if (c_1 <= 0.0) {
                throw new RuntimeException(String.valueOf("Capacitor at index " + _p(i_1) + " has a negative or zero value!"));
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
        main();
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
