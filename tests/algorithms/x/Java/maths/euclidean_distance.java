public class Main {

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double euclidean_distance(double[] v1, double[] v2) {
        double sum = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(v1.length)) {
            double diff_1 = (double)((double)(v1[(int)((long)(i_3))]) - (double)(v2[(int)((long)(i_3))]));
            sum = (double)((double)(sum) + (double)((double)(diff_1) * (double)(diff_1)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return sqrtApprox((double)(sum));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
