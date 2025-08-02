public class Main {
    static double PI;
    static class Complex {
        double re;
        double im;
        Complex(double re, double im) {
            this.re = re;
            this.im = im;
        }
        @Override public String toString() {
            return String.format("{'re': %s, 'im': %s}", String.valueOf(re), String.valueOf(im));
        }
    }


    static double sinApprox(double x) {
        double term = x;
        double sum = x;
        int n = 1;
        while (n <= 10) {
            double denom = ((Number)(((2 * n) * (2 * n + 1)))).doubleValue();
            term = -term * x * x / denom;
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double cosApprox(double x) {
        double term_1 = 1.0;
        double sum_1 = 1.0;
        int n_1 = 1;
        while (n_1 <= 10) {
            double denom_1 = ((Number)(((2 * n_1 - 1) * (2 * n_1)))).doubleValue();
            term_1 = -term_1 * x * x / denom_1;
            sum_1 = sum_1 + term_1;
            n_1 = n_1 + 1;
        }
        return sum_1;
    }

    static Complex cis(double x) {
        return new Complex(cosApprox(x), sinApprox(x));
    }

    static Complex add(Complex a, Complex b) {
        return new Complex(a.re + b.re, a.im + b.im);
    }

    static Complex sub(Complex a, Complex b) {
        return new Complex(a.re - b.re, a.im - b.im);
    }

    static Complex mul(Complex a, Complex b) {
        return new Complex(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re);
    }

    static void ditfft2Rec(double[] x, Complex[] y, int offX, int offY, int n, int s) {
        if (n == 1) {
y[offY] = new Complex(x[offX], 0.0);
            return;
        }
        ditfft2Rec(x, y, offX, offY, n / 2, 2 * s);
        ditfft2Rec(x, y, offX + s, offY + n / 2, n / 2, 2 * s);
        int k = 0;
        while (k < n / 2) {
            double angle = -2.0 * PI * (((Number)(k)).doubleValue()) / (((Number)(n)).doubleValue());
            Complex tf = mul(cis(angle), y[offY + k + n / 2]);
            Complex a = add(y[offY + k], tf);
            Complex b = sub(y[offY + k], tf);
y[offY + k] = a;
y[offY + k + n / 2] = b;
            k = k + 1;
        }
    }

    static void ditfft2(double[] x, Complex[] y, int n, int s) {
        ditfft2Rec(x, y, 0, 0, n, s);
    }

    static void main() {
        double[] x = new double[]{1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0};
        Complex[] y = new Complex[]{};
        int i = 0;
        while (i < x.length) {
            y = java.util.stream.Stream.concat(java.util.Arrays.stream(y), java.util.stream.Stream.of(new Complex(0.0, 0.0))).toArray(Complex[]::new);
            i = i + 1;
        }
        ditfft2(x, y, x.length, 1);
        for (Complex c : y) {
            String line = String.valueOf(pad(String.valueOf(fmt(c.re)), 8));
            if (c.im >= 0) {
                line = line + "+" + String.valueOf(fmt(c.im));
            } else {
                line = line + String.valueOf(fmt(c.im));
            }
            System.out.println(line);
        }
    }

    static String pad(String s, int w) {
        String t = s;
        while (_runeLen(t) < w) {
            t = " " + t;
        }
        return t;
    }

    static String fmt(double x) {
        double y_1 = floorf(x * 10000.0 + 0.5) / 10000.0;
        String s = (String)(_p(y_1));
        int dot = ((Number)(s.indexOf("."))).intValue();
        if (dot == 0 - 1) {
            s = s + ".0000";
        } else {
            int d = _runeLen(s) - dot - 1;
            while (d < 4) {
                s = s + "0";
                d = d + 1;
            }
        }
        return s;
    }

    static double floorf(double x) {
        int y_2 = ((Number)(x)).intValue();
        return ((Number)(y_2)).doubleValue();
    }

    static int indexOf(String s, String ch) {
        int i_1 = 0;
        while (i_1 < _runeLen(s)) {
            if ((_substr(s, i_1, i_1 + 1).equals(ch))) {
                return i_1;
            }
            i_1 = i_1 + 1;
        }
        return 0 - 1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
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

    static int _runeLen(String s) {
        return s.codePointCount(0, s.length());
    }

    static String _substr(String s, int i, int j) {
        int start = s.offsetByCodePoints(0, i);
        int end = s.offsetByCodePoints(0, j);
        return s.substring(start, end);
    }

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
