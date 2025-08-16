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

    static double pow_float(double base, long exp) {
        double res = (double)(1.0);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            res = (double)((double)(res) * (double)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return res;
    }

    static Dual add(Dual a, Dual b) {
        return new Dual((double)(a.value) + (double)(b.value), (double)(a.deriv) + (double)(b.deriv));
    }

    static Dual sub(Dual a, Dual b) {
        return new Dual((double)(a.value) - (double)(b.value), (double)(a.deriv) - (double)(b.deriv));
    }

    static Dual mul(Dual a, Dual b) {
        return new Dual((double)(a.value) * (double)(b.value), (double)((double)(a.deriv) * (double)(b.value)) + (double)((double)(b.deriv) * (double)(a.value)));
    }

    static Dual div(Dual a, Dual b) {
        return new Dual((double)(a.value) / (double)(b.value), (double)(((double)((double)(a.deriv) * (double)(b.value)) - (double)((double)(b.deriv) * (double)(a.value)))) / (double)(((double)(b.value) * (double)(b.value))));
    }

    static Dual power(Dual a, long p) {
        return new Dual(pow_float((double)(a.value), (long)(p)), (double)((double)(((double)(1.0) * (double)(p))) * (double)(pow_float((double)(a.value), (long)((long)(p) - 1L)))) * (double)(a.deriv));
    }

    static void main() {
        Dual a = dual((double)(2.0), (double)(1.0));
        Dual b_1 = dual((double)(1.0), (double)(0.0));
        Dual c_1 = add(a, b_1);
        Dual d_1 = mul(a, b_1);
        Dual e_1 = div(c_1, d_1);
        System.out.println(_p(e_1.deriv));
        Dual x_1 = dual((double)(2.0), (double)(1.0));
        Dual y_1 = power(x_1, 3L);
        System.out.println(_p(y_1.deriv));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
