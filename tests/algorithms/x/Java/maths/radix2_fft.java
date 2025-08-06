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

    static double PI;
    static double[] A;
    static double[] B;
    static double[] product;

    static Complex c_add(Complex a, Complex b) {
        return new Complex(a.re + b.re, a.im + b.im);
    }

    static Complex c_sub(Complex a, Complex b) {
        return new Complex(a.re - b.re, a.im - b.im);
    }

    static Complex c_mul(Complex a, Complex b) {
        return new Complex(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re);
    }

    static Complex c_mul_scalar(Complex a, double s) {
        return new Complex(a.re * s, a.im * s);
    }

    static Complex c_div_scalar(Complex a, double s) {
        return new Complex(a.re / s, a.im / s);
    }

    static double sin_taylor(double x) {
        double term = x;
        double sum = x;
        int i = 1;
        while (i < 10) {
            double k1 = 2.0 * (((Number)(i)).doubleValue());
            double k2 = k1 + 1.0;
            term = -term * x * x / (k1 * k2);
            sum = sum + term;
            i = i + 1;
        }
        return sum;
    }

    static double cos_taylor(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int i_1 = 1;
        while (i_1 < 10) {
            double k1_1 = 2.0 * (((Number)(i_1)).doubleValue()) - 1.0;
            double k2_1 = 2.0 * (((Number)(i_1)).doubleValue());
            term_1 = -term_1 * x * x / (k1_1 * k2_1);
            sum_1 = sum_1 + term_1;
            i_1 = i_1 + 1;
        }
        return sum_1;
    }

    static Complex exp_i(double theta) {
        return new Complex(cos_taylor(theta), sin_taylor(theta));
    }

    static Complex[] make_complex_list(int n, Complex value) {
        Complex[] arr = ((Complex[])(new Complex[]{}));
        int i_2 = 0;
        while (i_2 < n) {
            arr = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.stream.Stream.of(value)).toArray(Complex[]::new)));
            i_2 = i_2 + 1;
        }
        return arr;
    }

    static Complex[] fft(Complex[] a, boolean invert) {
        int n = a.length;
        if (n == 1) {
            return new Complex[]{a[0]};
        }
        Complex[] a0 = ((Complex[])(new Complex[]{}));
        Complex[] a1 = ((Complex[])(new Complex[]{}));
        int i_3 = 0;
        while (i_3 < n / 2) {
            a0 = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(a0), java.util.stream.Stream.of(a[2 * i_3])).toArray(Complex[]::new)));
            a1 = ((Complex[])(java.util.stream.Stream.concat(java.util.Arrays.stream(a1), java.util.stream.Stream.of(a[2 * i_3 + 1])).toArray(Complex[]::new)));
            i_3 = i_3 + 1;
        }
        Complex[] y0 = ((Complex[])(fft(((Complex[])(a0)), invert)));
        Complex[] y1 = ((Complex[])(fft(((Complex[])(a1)), invert)));
        double angle = 2.0 * PI / (((Number)(n)).doubleValue()) * (invert ? -1.0 : 1.0);
        Complex w = new Complex(1.0, 0.0);
        Complex wn = exp_i(angle);
        Complex[] y = ((Complex[])(make_complex_list(n, new Complex(0.0, 0.0))));
        i_3 = 0;
        while (i_3 < n / 2) {
            Complex t = c_mul(w, y1[i_3]);
            Complex u = y0[i_3];
            Complex even = c_add(u, t);
            Complex odd = c_sub(u, t);
            if (((Boolean)(invert))) {
                even = c_div_scalar(even, 2.0);
                odd = c_div_scalar(odd, 2.0);
            }
y[i_3] = even;
y[i_3 + n / 2] = odd;
            w = c_mul(w, wn);
            i_3 = i_3 + 1;
        }
        return y;
    }

    static double floor(double x) {
        int i_4 = ((Number)(x)).intValue();
        if ((((Number)(i_4)).doubleValue()) > x) {
            i_4 = i_4 - 1;
        }
        return ((Number)(i_4)).doubleValue();
    }

    static double pow10(int n) {
        double p = 1.0;
        int i_5 = 0;
        while (i_5 < n) {
            p = p * 10.0;
            i_5 = i_5 + 1;
        }
        return p;
    }

    static double round_to(double x, int ndigits) {
        double m = pow10(ndigits);
        return floor(x * m + 0.5) / m;
    }

    static String list_to_string(double[] l) {
        String s = "[";
        int i_6 = 0;
        while (i_6 < l.length) {
            s = s + _p(_geto(l, i_6));
            if (i_6 + 1 < l.length) {
                s = s + ", ";
            }
            i_6 = i_6 + 1;
        }
        s = s + "]";
        return s;
    }

    static double[] multiply_poly(double[] a, double[] b) {
        int n_1 = 1;
        while (n_1 < a.length + b.length - 1) {
            n_1 = n_1 * 2;
        }
        Complex[] fa = ((Complex[])(make_complex_list(n_1, new Complex(0.0, 0.0))));
        Complex[] fb = ((Complex[])(make_complex_list(n_1, new Complex(0.0, 0.0))));
        int i_7 = 0;
        while (i_7 < a.length) {
fa[i_7] = new Complex(a[i_7], 0.0);
            i_7 = i_7 + 1;
        }
        i_7 = 0;
        while (i_7 < b.length) {
fb[i_7] = new Complex(b[i_7], 0.0);
            i_7 = i_7 + 1;
        }
        fa = ((Complex[])(fft(((Complex[])(fa)), false)));
        fb = ((Complex[])(fft(((Complex[])(fb)), false)));
        i_7 = 0;
        while (i_7 < n_1) {
fa[i_7] = c_mul(fa[i_7], fb[i_7]);
            i_7 = i_7 + 1;
        }
        fa = ((Complex[])(fft(((Complex[])(fa)), true)));
        double[] res = ((double[])(new double[]{}));
        i_7 = 0;
        while (i_7 < a.length + b.length - 1) {
            Complex val = fa[i_7];
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(round_to(val.re, 8))).toArray()));
            i_7 = i_7 + 1;
        }
        while (res.length > 0 && res[res.length - 1] == 0.0) {
            res = ((double[])(java.util.Arrays.copyOfRange(res, 0, res.length - 1)));
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            A = ((double[])(new double[]{0.0, 1.0, 0.0, 2.0}));
            B = ((double[])(new double[]{2.0, 3.0, 4.0, 0.0}));
            product = ((double[])(multiply_poly(((double[])(A)), ((double[])(B)))));
            System.out.println(list_to_string(((double[])(product))));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
