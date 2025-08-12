public class Main {
    static double PI;

    static double floor(double x) {
        long i = ((Number)(x)).intValue();
        if ((((Number)(i)).doubleValue()) > x) {
            i = i - 1;
        }
        return ((Number)(i)).doubleValue();
    }

    static double modf(double x, double m) {
        return x - floor(x / m) * m;
    }

    static double sin_taylor(double angle) {
        double x = modf(angle, 2.0 * PI);
        if (x > PI) {
            x = x - 2.0 * PI;
        }
        double term_1 = x;
        double sum_1 = x;
        long i_2 = 1L;
        while (i_2 < 10) {
            double k1_1 = 2.0 * (((Number)(i_2)).doubleValue());
            double k2_1 = k1_1 + 1.0;
            term_1 = -term_1 * x * x / (k1_1 * k2_1);
            sum_1 = sum_1 + term_1;
            i_2 = i_2 + 1;
        }
        return sum_1;
    }

    static double cos_taylor(double angle) {
        double x_1 = modf(angle, 2.0 * PI);
        if (x_1 > PI) {
            x_1 = x_1 - 2.0 * PI;
        }
        double term_3 = 1.0;
        double sum_3 = 1.0;
        long i_4 = 1L;
        while (i_4 < 10) {
            double k1_3 = 2.0 * (((Number)(i_4)).doubleValue()) - 1.0;
            double k2_3 = 2.0 * (((Number)(i_4)).doubleValue());
            term_3 = -term_3 * x_1 * x_1 / (k1_3 * k2_3);
            sum_3 = sum_3 + term_3;
            i_4 = i_4 + 1;
        }
        return sum_3;
    }

    static String matrix_to_string(double[][] m) {
        String s = "[";
        long i_6 = 0L;
        while (i_6 < m.length) {
            double[] row_1 = ((double[])(m[(int)((long)(i_6))]));
            s = s + "[";
            long j_1 = 0L;
            while (j_1 < row_1.length) {
                s = s + _p(_getd(row_1, ((Number)(j_1)).intValue()));
                if (j_1 < row_1.length - 1) {
                    s = s + ", ";
                }
                j_1 = j_1 + 1;
            }
            s = s + "]";
            if (i_6 < m.length - 1) {
                s = s + ", ";
            }
            i_6 = i_6 + 1;
        }
        s = s + "]";
        return s;
    }

    static double[][] scaling(double f) {
        return ((double[][])(new Object[][]{new Object[]{f, 0.0}, new Object[]{0.0, f}}));
    }

    static double[][] rotation(double angle) {
        double c = cos_taylor(angle);
        double s_2 = sin_taylor(angle);
        return new double[][]{new double[]{c, -s_2}, new double[]{s_2, c}};
    }

    static double[][] projection(double angle) {
        double c_1 = cos_taylor(angle);
        double s_4 = sin_taylor(angle);
        double cs_1 = c_1 * s_4;
        return new double[][]{new double[]{c_1 * c_1, cs_1}, new double[]{cs_1, s_4 * s_4}};
    }

    static double[][] reflection(double angle) {
        double c_2 = cos_taylor(angle);
        double s_6 = sin_taylor(angle);
        double cs_3 = c_2 * s_6;
        return new double[][]{new double[]{2.0 * c_2 - 1.0, 2.0 * cs_3}, new double[]{2.0 * cs_3, 2.0 * s_6 - 1.0}};
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        System.out.println("    scaling(5) = " + String.valueOf(matrix_to_string(((double[][])(scaling(5.0))))));
        System.out.println("  rotation(45) = " + String.valueOf(matrix_to_string(((double[][])(rotation(45.0))))));
        System.out.println("projection(45) = " + String.valueOf(matrix_to_string(((double[][])(projection(45.0))))));
        System.out.println("reflection(45) = " + String.valueOf(matrix_to_string(((double[][])(reflection(45.0))))));
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
