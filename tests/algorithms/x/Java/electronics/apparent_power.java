public class Main {
    static double PI = (double)(3.141592653589793);

    static double abs(double x) {
        if ((double)(x) < (double)(0.0)) {
            return (double)(-x);
        }
        return (double)(x);
    }

    static double to_radians(double deg) {
        return (double)((double)((double)(deg) * (double)(PI)) / (double)(180.0));
    }

    static double sin_taylor(double x) {
        double term = (double)(x);
        double sum_1 = (double)(x);
        java.math.BigInteger i_1 = java.math.BigInteger.valueOf(1);
        while (i_1.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            double k1_1 = (double)((double)(2.0) * (double)((((Number)(i_1)).doubleValue())));
            double k2_1 = (double)((double)(k1_1) + (double)(1.0));
            term = (double)((double)((double)((double)(-term) * (double)(x)) * (double)(x)) / (double)(((double)(k1_1) * (double)(k2_1))));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_1 = new java.math.BigInteger(String.valueOf(i_1.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(sum_1);
    }

    static double cos_taylor(double x) {
        double term_1 = (double)(1.0);
        double sum_3 = (double)(1.0);
        java.math.BigInteger i_3 = java.math.BigInteger.valueOf(1);
        while (i_3.compareTo(java.math.BigInteger.valueOf(10)) < 0) {
            double k1_3 = (double)((double)((double)(2.0) * (double)((((Number)(i_3)).doubleValue()))) - (double)(1.0));
            double k2_3 = (double)((double)(2.0) * (double)((((Number)(i_3)).doubleValue())));
            term_1 = (double)((double)((double)((double)(-term_1) * (double)(x)) * (double)(x)) / (double)(((double)(k1_3) * (double)(k2_3))));
            sum_3 = (double)((double)(sum_3) + (double)(term_1));
            i_3 = new java.math.BigInteger(String.valueOf(i_3.add(java.math.BigInteger.valueOf(1))));
        }
        return (double)(sum_3);
    }

    static double[] rect(double mag, double angle) {
        double c = (double)(cos_taylor((double)(angle)));
        double s_1 = (double)(sin_taylor((double)(angle)));
        return ((double[])(new double[]{(double)((double)(mag) * (double)(c)), (double)((double)(mag) * (double)(s_1))}));
    }

    static double[] multiply(double[] a, double[] b) {
        return ((double[])(new double[]{(double)((double)((double)(a[_idx((a).length, 0L)]) * (double)(b[_idx((b).length, 0L)])) - (double)((double)(a[_idx((a).length, 1L)]) * (double)(b[_idx((b).length, 1L)]))), (double)((double)((double)(a[_idx((a).length, 0L)]) * (double)(b[_idx((b).length, 1L)])) + (double)((double)(a[_idx((a).length, 1L)]) * (double)(b[_idx((b).length, 0L)])))}));
    }

    static double[] apparent_power(double voltage, double current, double voltage_angle, double current_angle) {
        double vrad = (double)(to_radians((double)(voltage_angle)));
        double irad_1 = (double)(to_radians((double)(current_angle)));
        double[] vrect_1 = ((double[])(rect((double)(voltage), (double)(vrad))));
        double[] irect_1 = ((double[])(rect((double)(current), (double)(irad_1))));
        double[] result_1 = ((double[])(multiply(((double[])(vrect_1)), ((double[])(irect_1)))));
        return ((double[])(result_1));
    }

    static boolean approx_equal(double[] a, double[] b, double eps) {
        return Math.abs((double)(a[_idx((a).length, 0L)]) - (double)(b[_idx((b).length, 0L)])) < (double)(eps) && Math.abs((double)(a[_idx((a).length, 1L)]) - (double)(b[_idx((b).length, 1L)])) < (double)(eps);
    }
    public static void main(String[] args) {
    }

    static int _idx(int len, long i) {
        return (int)(i < 0 ? len + i : i);
    }
}
