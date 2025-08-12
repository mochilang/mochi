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
        double res = 1.0;
        long i_1 = 0L;
        while (i_1 < exp) {
            res = res * base;
            i_1 = i_1 + 1;
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

    static Dual power(Dual a, long p) {
        return new Dual(pow_float(a.value, p), (1.0 * p) * pow_float(a.value, p - 1) * a.deriv);
    }

    static void main() {
        Dual a = dual(2.0, 1.0);
        Dual b_1 = dual(1.0, 0.0);
        Dual c_1 = add(a, b_1);
        Dual d_1 = mul(a, b_1);
        Dual e_1 = div(c_1, d_1);
        System.out.println(_p(e_1.deriv));
        Dual x_1 = dual(2.0, 1.0);
        Dual y_1 = power(x_1, 3L);
        System.out.println(_p(y_1.deriv));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
