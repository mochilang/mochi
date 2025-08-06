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
        return new Complex(a.re + b.re, a.im + b.im);
    }

    static Complex sub(Complex a, Complex b) {
        return new Complex(a.re - b.re, a.im - b.im);
    }

    static Complex div_real(Complex a, double r) {
        return new Complex(a.re / r, a.im / r);
    }

    static double sqrt_newton(double x) {
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

    static Complex sqrt_to_complex(double d) {
        if (d >= 0.0) {
            return new Complex(sqrt_newton(d), 0.0);
        }
        return new Complex(0.0, sqrt_newton(-d));
    }

    static Complex[] quadratic_roots(double a, double b, double c) {
        if (a == 0.0) {
            System.out.println("ValueError: coefficient 'a' must not be zero");
            return new Complex[]{};
        }
        double delta = b * b - 4.0 * a * c;
        Complex sqrt_d = sqrt_to_complex(delta);
        Complex minus_b = new Complex(-b, 0.0);
        double two_a = 2.0 * a;
        Complex root1 = div_real(add(minus_b, sqrt_d), two_a);
        Complex root2 = div_real(sub(minus_b, sqrt_d), two_a);
        return new Complex[]{root1, root2};
    }

    static String root_str(Complex r) {
        if (r.im == 0.0) {
            return _p(r.re);
        }
        String s = _p(r.re);
        if (r.im >= 0.0) {
            s = s + "+" + _p(r.im) + "i";
        } else {
            s = s + _p(r.im) + "i";
        }
        return s;
    }

    static void main() {
        Complex[] roots = ((Complex[])(quadratic_roots(5.0, 6.0, 1.0)));
        if (roots.length == 2) {
            System.out.println("The solutions are: " + String.valueOf(root_str(roots[0])) + " and " + String.valueOf(root_str(roots[1])));
        }
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
