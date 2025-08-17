public class Main {
    static class Complex {
        double re;
        double im;
        Complex(double re, double im) {
            this.re = re;
            this.im = im;
        }
        Complex() {}
        @Override public String toString() {
            return String.format("{'re': %s, 'im': %s}", String.valueOf(re), String.valueOf(im));
        }
    }

    static double PI = (double)(3.141592653589793);
    static double[] A = ((double[])(new double[]{0.0, 1.0, 0.0, 2.0}));
    static double[] B = ((double[])(new double[]{2.0, 3.0, 4.0, 0.0}));
    static double[] product;

    static Complex c_add(Complex a, Complex b) {
        return new Complex((double)(a.re) + (double)(b.re), (double)(a.im) + (double)(b.im));
    }

    static Complex c_sub(Complex a, Complex b) {
        return new Complex((double)(a.re) - (double)(b.re), (double)(a.im) - (double)(b.im));
    }

    static Complex c_mul(Complex a, Complex b) {
        return new Complex((double)((double)(a.re) * (double)(b.re)) - (double)((double)(a.im) * (double)(b.im)), (double)((double)(a.re) * (double)(b.im)) + (double)((double)(a.im) * (double)(b.re)));
    }

    static Complex c_mul_scalar(Complex a, double s) {
        return new Complex((double)(a.re) * (double)(s), (double)(a.im) * (double)(s));
    }

    static Complex c_div_scalar(Complex a, double s) {
        return new Complex((double)(a.re) / (double)(s), (double)(a.im) / (double)(s));
    }

    static double sin_taylor(double x) {
        double term = (double)(x);
        double sum_1 = (double)(x);
        long i_1 = 1L;
        while ((long)(i_1) < 10L) {
            double k1_1 = (double)((double)(2.0) * (double)((((Number)(i_1)).doubleValue())));
            double k2_1 = (double)((double)(k1_1) + (double)(1.0));
            term = (double)((double)((double)((double)(-term) * (double)(x)) * (double)(x)) / (double)(((double)(k1_1) * (double)(k2_1))));
            sum_1 = (double)((double)(sum_1) + (double)(term));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return sum_1;
    }

    static double cos_taylor(double x) {
        double term_1 = (double)(1.0);
        double sum_3 = (double)(1.0);
        long i_3 = 1L;
        while ((long)(i_3) < 10L) {
            double k1_3 = (double)((double)((double)(2.0) * (double)((((Number)(i_3)).doubleValue()))) - (double)(1.0));
            double k2_3 = (double)((double)(2.0) * (double)((((Number)(i_3)).doubleValue())));
            term_1 = (double)((double)((double)((double)(-term_1) * (double)(x)) * (double)(x)) / (double)(((double)(k1_3) * (double)(k2_3))));
            sum_3 = (double)((double)(sum_3) + (double)(term_1));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return sum_3;
    }

    static Complex exp_i(double theta) {
        return new Complex(cos_taylor((double)(theta)), sin_taylor((double)(theta)));
    }

    static Complex[] make_complex_list(long n, Complex value) {
        Complex[] arr = ((Complex[])(new Complex[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(n)) {
            arr = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(value)).toArray(Complex[]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return arr;
    }

    static Complex[] fft(Complex[] a, boolean invert) {
        long n = (long)(a.length);
        if ((long)(n) == 1L) {
            return new Complex[]{a[(int)(0L)]};
        }
        Complex[] a0_1 = ((Complex[])(new Complex[]{}));
        Complex[] a1_1 = ((Complex[])(new Complex[]{}));
        long i_7 = 0L;
        while ((long)(i_7) < Math.floorDiv(n, 2)) {
            a0_1 = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(a0_1), java.util.stream.Stream.of(a[(int)((long)(2L * (long)(i_7)))])).toArray(Complex[]::new)));
            a1_1 = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(a1_1), java.util.stream.Stream.of(a[(int)((long)((long)(2L * (long)(i_7)) + 1L))])).toArray(Complex[]::new)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        Complex[] y0_1 = ((Complex[])(fft(((Complex[])(a0_1)), invert)));
        Complex[] y1_1 = ((Complex[])(fft(((Complex[])(a1_1)), invert)));
        double angle_1 = (double)((double)((double)((double)(2.0) * (double)(PI)) / (double)((((Number)(n)).doubleValue()))) * (double)((invert ? -1.0 : 1.0)));
        Complex w_1 = new Complex(1.0, 0.0);
        Complex wn_1 = exp_i((double)(angle_1));
        Complex[] y_1 = ((Complex[])(make_complex_list((long)(n), new Complex(0.0, 0.0))));
        i_7 = 0L;
        while ((long)(i_7) < Math.floorDiv(n, 2)) {
            Complex t_1 = c_mul(w_1, y1_1[(int)((long)(i_7))]);
            Complex u_1 = y0_1[(int)((long)(i_7))];
            Complex even_1 = c_add(u_1, t_1);
            Complex odd_1 = c_sub(u_1, t_1);
            if (invert) {
                even_1 = c_div_scalar(even_1, (double)(2.0));
                odd_1 = c_div_scalar(odd_1, (double)(2.0));
            }
y_1[(int)((long)(i_7))] = even_1;
y_1[(int)((long)((long)(i_7) + Math.floorDiv(n, 2)))] = odd_1;
            w_1 = c_mul(w_1, wn_1);
            i_7 = (long)((long)(i_7) + 1L);
        }
        return y_1;
    }

    static double floor(double x) {
        long i_8 = (long)(((Number)(x)).intValue());
        if ((double)((((Number)(i_8)).doubleValue())) > (double)(x)) {
            i_8 = (long)((long)(i_8) - 1L);
        }
        return ((Number)(i_8)).doubleValue();
    }

    static double pow10(long n) {
        double p = (double)(1.0);
        long i_10 = 0L;
        while ((long)(i_10) < (long)(n)) {
            p = (double)((double)(p) * (double)(10.0));
            i_10 = (long)((long)(i_10) + 1L);
        }
        return p;
    }

    static double round_to(double x, long ndigits) {
        double m = (double)(pow10((long)(ndigits)));
        return Math.floor((double)((double)(x) * (double)(m)) + (double)(0.5)) / (double)(m);
    }

    static String list_to_string(double[] l) {
        String s = "[";
        long i_12 = 0L;
        while ((long)(i_12) < (long)(l.length)) {
            s = s + _p(_getd(l, ((Number)(i_12)).intValue()));
            if ((long)((long)(i_12) + 1L) < (long)(l.length)) {
                s = s + ", ";
            }
            i_12 = (long)((long)(i_12) + 1L);
        }
        s = s + "]";
        return s;
    }

    static double[] multiply_poly(double[] a, double[] b) {
        long n_1 = 1L;
        while ((long)(n_1) < (long)((long)((long)(a.length) + (long)(b.length)) - 1L)) {
            n_1 = (long)((long)(n_1) * 2L);
        }
        Complex[] fa_1 = ((Complex[])(make_complex_list((long)(n_1), new Complex(0.0, 0.0))));
        Complex[] fb_1 = ((Complex[])(make_complex_list((long)(n_1), new Complex(0.0, 0.0))));
        long i_14 = 0L;
        while ((long)(i_14) < (long)(a.length)) {
fa_1[(int)((long)(i_14))] = new Complex(a[(int)((long)(i_14))], 0.0);
            i_14 = (long)((long)(i_14) + 1L);
        }
        i_14 = 0L;
        while ((long)(i_14) < (long)(b.length)) {
fb_1[(int)((long)(i_14))] = new Complex(b[(int)((long)(i_14))], 0.0);
            i_14 = (long)((long)(i_14) + 1L);
        }
        fa_1 = ((Complex[])(fft(((Complex[])(fa_1)), false)));
        fb_1 = ((Complex[])(fft(((Complex[])(fb_1)), false)));
        i_14 = 0L;
        while ((long)(i_14) < (long)(n_1)) {
fa_1[(int)((long)(i_14))] = c_mul(fa_1[(int)((long)(i_14))], fb_1[(int)((long)(i_14))]);
            i_14 = (long)((long)(i_14) + 1L);
        }
        fa_1 = ((Complex[])(fft(((Complex[])(fa_1)), true)));
        double[] res_1 = ((double[])(new double[]{}));
        i_14 = 0L;
        while ((long)(i_14) < (long)((long)((long)(a.length) + (long)(b.length)) - 1L)) {
            Complex val_1 = fa_1[(int)((long)(i_14))];
            res_1 = ((double[])(appendDouble(res_1, (double)(round_to((double)(val_1.re), 8L)))));
            i_14 = (long)((long)(i_14) + 1L);
        }
        while ((long)(res_1.length) > 0L && (double)(res_1[(int)((long)((long)(res_1.length) - 1L))]) == (double)(0.0)) {
            res_1 = ((double[])(java.util.Arrays.copyOfRange(res_1, (int)(0L), (int)((long)((long)(res_1.length) - 1L)))));
        }
        return res_1;
    }
    public static void main(String[] args) {
        product = ((double[])(multiply_poly(((double[])(A)), ((double[])(B)))));
        System.out.println(list_to_string(((double[])(product))));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
