public class Main {
    static class Dual {
        double value;
        double deriv;
        Dual(double value, double deriv) {
            this.value = value;
            this.deriv = deriv;
        }
        Dual() {}
        @Override public String toString() {
            return String.format("{'value': %s, 'deriv': %s}", String.valueOf(value), String.valueOf(deriv));
        }
    }


    static Dual dual(double v, double d) {
        return new Dual(v, d);
    }

    static double pow_float(double base, int exp) {
        double res = 1.0;
        int i = 0;
        while (i < exp) {
            res = res * base;
            i = i + 1;
        }
        return res;
    }

    static Dual add(Dual a, Dual b) {
        return new Dual(a.value + b.value, a.deriv + b.deriv);
    }

    static Dual sub(Dual a, Dual b) {
        return new Dual(a.value - b.value, a.deriv - b.deriv);
    }

    static Dual mul(Dual a, Dual b) {
        return new Dual(a.value * b.value, a.deriv * b.value + b.deriv * a.value);
    }

    static Dual div(Dual a, Dual b) {
        return new Dual(a.value / b.value, (a.deriv * b.value - b.deriv * a.value) / (b.value * b.value));
    }

    static Dual power(Dual a, int p) {
        return new Dual(pow_float(a.value, p), (1.0 * p) * pow_float(a.value, p - 1) * a.deriv);
    }

    static void main() {
        Dual a = dual(2.0, 1.0);
        Dual b = dual(1.0, 0.0);
        Dual c = add(a, b);
        Dual d = mul(a, b);
        Dual e = div(c, d);
        System.out.println(_p(e.deriv));
        Dual x = dual(2.0, 1.0);
        Dual y = power(x, 3);
        System.out.println(_p(y.deriv));
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
