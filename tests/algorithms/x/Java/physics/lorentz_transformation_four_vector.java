public class Main {
    static double c;
    static double[] v;

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double beta(double velocity) {
        if (velocity > c) {
            throw new RuntimeException(String.valueOf("Speed must not exceed light speed 299,792,458 [m/s]!"));
        }
        if (velocity < 1.0) {
            throw new RuntimeException(String.valueOf("Speed must be greater than or equal to 1!"));
        }
        return velocity / c;
    }

    static double gamma(double velocity) {
        double b = beta(velocity);
        return 1.0 / sqrtApprox(1.0 - b * b);
    }

    static double[][] transformation_matrix(double velocity) {
        double g = gamma(velocity);
        double b_1 = beta(velocity);
        return new double[][]{new Object[]{g, -g * b_1, 0.0, 0.0}, new Object[]{-g * b_1, g, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 0.0, 1.0}};
    }

    static double[] mat_vec_mul(double[][] mat, double[] vec) {
        double[] res = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < 4) {
            double[] row = ((double[])(mat[i_1]));
            double value = row[0] * vec[0] + row[1] * vec[1] + row[2] * vec[2] + row[3] * vec[3];
            res = ((double[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new double[]{value})).toArray(double[]::new)));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static double[] transform(double velocity, double[] event) {
        double g_1 = gamma(velocity);
        double b_2 = beta(velocity);
        double ct = event[0] * c;
        double x = event[1];
        return new double[]{g_1 * ct - g_1 * b_2 * x, -g_1 * b_2 * ct + g_1 * x, event[2], event[3]};
    }
    public static void main(String[] args) {
        c = 299792458.0;
        System.out.println(_p(beta(c)));
        System.out.println(_p(beta(199792458.0)));
        System.out.println(_p(beta(100000.0)));
        System.out.println(_p(gamma(4.0)));
        System.out.println(_p(gamma(100000.0)));
        System.out.println(_p(gamma(30000000.0)));
        System.out.println(_p(transformation_matrix(29979245.0)));
        v = ((double[])(transform(29979245.0, ((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
        System.out.println(_p(v));
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
