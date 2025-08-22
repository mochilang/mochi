public class Main {
    static double c = (double)(299792458.0);
    static double[] v;

    static double sqrtApprox(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)((double)(x) / (double)(2.0));
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double beta(double velocity) {
        if ((double)(velocity) > (double)(c)) {
            throw new RuntimeException(String.valueOf("Speed must not exceed light speed 299,792,458 [m/s]!"));
        }
        if ((double)(velocity) < (double)(1.0)) {
            throw new RuntimeException(String.valueOf("Speed must be greater than or equal to 1!"));
        }
        return (double)(velocity) / (double)(c);
    }

    static double gamma(double velocity) {
        double b = (double)(beta((double)(velocity)));
        return (double)(1.0) / (double)(sqrtApprox((double)((double)(1.0) - (double)((double)(b) * (double)(b)))));
    }

    static double[][] transformation_matrix(double velocity) {
        double g = (double)(gamma((double)(velocity)));
        double b_2 = (double)(beta((double)(velocity)));
        return new double[][]{new double[]{g, (double)(-g) * (double)(b_2), 0.0, 0.0}, new double[]{(double)(-g) * (double)(b_2), g, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0, 0.0}, new double[]{0.0, 0.0, 0.0, 1.0}};
    }

    static double[] mat_vec_mul(double[][] mat, double[] vec) {
        double[] res = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < 4L) {
            double[] row_1 = ((double[])(mat[(int)((long)(i_3))]));
            double value_1 = (double)((double)((double)((double)((double)(row_1[(int)(0L)]) * (double)(vec[(int)(0L)])) + (double)((double)(row_1[(int)(1L)]) * (double)(vec[(int)(1L)]))) + (double)((double)(row_1[(int)(2L)]) * (double)(vec[(int)(2L)]))) + (double)((double)(row_1[(int)(3L)]) * (double)(vec[(int)(3L)])));
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new double[]{value_1})).toArray()));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return res;
    }

    static double[] transform(double velocity, double[] event) {
        double g_1 = (double)(gamma((double)(velocity)));
        double b_4 = (double)(beta((double)(velocity)));
        double ct_1 = (double)((double)(event[(int)(0L)]) * (double)(c));
        double x_1 = (double)(event[(int)(1L)]);
        return new double[]{(double)((double)(g_1) * (double)(ct_1)) - (double)((double)((double)(g_1) * (double)(b_4)) * (double)(x_1)), (double)((double)((double)(-g_1) * (double)(b_4)) * (double)(ct_1)) + (double)((double)(g_1) * (double)(x_1)), event[(int)(2L)], event[(int)(3L)]};
    }
    public static void main(String[] args) {
        System.out.println(_p(beta((double)(c))));
        System.out.println(_p(beta((double)(199792458.0))));
        System.out.println(_p(beta((double)(100000.0))));
        System.out.println(_p(gamma((double)(4.0))));
        System.out.println(_p(gamma((double)(100000.0))));
        System.out.println(_p(gamma((double)(30000000.0))));
        System.out.println(_p(transformation_matrix((double)(29979245.0))));
        v = ((double[])(transform((double)(29979245.0), ((double[])(new double[]{1.0, 2.0, 3.0, 4.0})))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
