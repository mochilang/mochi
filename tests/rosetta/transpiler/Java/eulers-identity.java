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

    static String cstr(Complex a) {
        String s = "(" + (String)(_p(a.re));
        if (a.im >= 0) {
            s = s + "+" + (String)(_p(a.im)) + "i)";
        } else {
            s = s + (String)(_p(a.im)) + "i)";
        }
        return s;
    }

    static void main() {
        Complex result = add(cis(PI), new Complex(1.0, 0.0));
        System.out.println(cstr(result));
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

    static String _p(Object v) {
        return v != null ? String.valueOf(v) : "<nil>";
    }
}
