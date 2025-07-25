public class Main {
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

    static Complex a = new Complex(1.0, 1.0);
    static Complex b = new Complex(3.14159, 1.25);

    static Complex add(Complex a, Complex b) {
        return new Complex(a.re + b.re, a.im + b.im);
    }

    static Complex mul(Complex a, Complex b) {
        return new Complex(a.re * b.re - a.im * b.im, a.re * b.im + a.im * b.re);
    }

    static Complex neg(Complex a) {
        return new Complex(-a.re, -a.im);
    }

    static Complex inv(Complex a) {
        double denom = a.re * a.re + a.im * a.im;
        return new Complex(a.re / denom, -a.im / denom);
    }

    static Complex conj(Complex a) {
        return new Complex(a.re, -a.im);
    }

    static String cstr(Complex a) {
        String s = String.valueOf("(" + String.valueOf(a.re));
        if (a.im >= 0) {
            s = String.valueOf(String.valueOf(String.valueOf(s + "+") + String.valueOf(a.im)) + "i)");
        } else {
            s = String.valueOf(String.valueOf(s + String.valueOf(a.im)) + "i)");
        }
        return s;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println("a:       " + String.valueOf(cstr(a)));
            System.out.println("b:       " + String.valueOf(cstr(b)));
            System.out.println("a + b:   " + String.valueOf(cstr(add(a, b))));
            System.out.println("a * b:   " + String.valueOf(cstr(mul(a, b))));
            System.out.println("-a:      " + String.valueOf(cstr(neg(a))));
            System.out.println("1 / a:   " + String.valueOf(cstr(inv(a))));
            System.out.println("a̅:       " + String.valueOf(cstr(conj(a))));
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
}
