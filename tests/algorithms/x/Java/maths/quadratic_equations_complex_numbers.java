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


    static Complex add(Complex a, Complex b) {
        return new Complex((double)(a.re) + (double)(b.re), (double)(a.im) + (double)(b.im));
    }

    static Complex sub(Complex a, Complex b) {
        return new Complex((double)(a.re) - (double)(b.re), (double)(a.im) - (double)(b.im));
    }

    static Complex div_real(Complex a, double r) {
        return new Complex((double)(a.re) / (double)(r), (double)(a.im) / (double)(r));
    }

    static double sqrt_newton(double x) {
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

    static Complex sqrt_to_complex(double d) {
        if ((double)(d) >= (double)(0.0)) {
            return new Complex(sqrt_newton((double)(d)), 0.0);
        }
        return new Complex(0.0, sqrt_newton((double)(-d)));
    }

    static Complex[] quadratic_roots(double a, double b, double c) {
        if ((double)(a) == (double)(0.0)) {
            System.out.println("ValueError: coefficient 'a' must not be zero");
            return new Complex[]{};
        }
        double delta_1 = (double)((double)((double)(b) * (double)(b)) - (double)((double)((double)(4.0) * (double)(a)) * (double)(c)));
        Complex sqrt_d_1 = sqrt_to_complex((double)(delta_1));
        Complex minus_b_1 = new Complex(-b, 0.0);
        double two_a_1 = (double)((double)(2.0) * (double)(a));
        Complex root1_1 = div_real(add(minus_b_1, sqrt_d_1), (double)(two_a_1));
        Complex root2_1 = div_real(sub(minus_b_1, sqrt_d_1), (double)(two_a_1));
        return new Complex[]{root1_1, root2_1};
    }

    static String root_str(Complex r) {
        if ((double)(r.im) == (double)(0.0)) {
            return _p(r.re);
        }
        String s_1 = _p(r.re);
        if ((double)(r.im) >= (double)(0.0)) {
            s_1 = s_1 + "+" + _p(r.im) + "i";
        } else {
            s_1 = s_1 + _p(r.im) + "i";
        }
        return s_1;
    }

    static void main() {
        Complex[] roots = ((Complex[])(quadratic_roots((double)(5.0), (double)(6.0), (double)(1.0))));
        if ((long)(roots.length) == 2L) {
            System.out.println("The solutions are: " + String.valueOf(root_str(roots[(int)(0L)])) + " and " + String.valueOf(root_str(roots[(int)(1L)])));
        }
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
