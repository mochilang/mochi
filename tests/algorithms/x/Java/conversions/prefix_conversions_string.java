public class Main {
    static class Prefix {
        String name;
        int exp;
        Prefix(String name, int exp) {
            this.name = name;
            this.exp = exp;
        }
        Prefix() {}
        @Override public String toString() {
            return String.format("{'name': '%s', 'exp': %s}", String.valueOf(name), String.valueOf(exp));
        }
    }

    static Prefix[] si_positive;
    static Prefix[] si_negative;
    static Prefix[] binary_prefixes;

    static double pow(double base, int exp) {
        double result = 1.0;
        int e = exp;
        if (e < 0) {
            e = -e;
            int i = 0;
            while (i < e) {
                result = result * base;
                i = i + 1;
            }
            return 1.0 / result;
        }
        int i_1 = 0;
        while (i_1 < e) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static String add_si_prefix(double value) {
        Prefix[] prefixes = new Prefix[0];
        if (value > 0.0) {
            prefixes = ((Prefix[])(si_positive));
        } else {
            prefixes = ((Prefix[])(si_negative));
        }
        int i_2 = 0;
        while (i_2 < prefixes.length) {
            Prefix p = prefixes[i_2];
            double num = value / pow(10.0, p.exp);
            if (num > 1.0) {
                return _p(num) + " " + p.name;
            }
            i_2 = i_2 + 1;
        }
        return _p(value);
    }

    static String add_binary_prefix(double value) {
        int i_3 = 0;
        while (i_3 < binary_prefixes.length) {
            Prefix p_1 = binary_prefixes[i_3];
            double num_1 = value / pow(2.0, p_1.exp);
            if (num_1 > 1.0) {
                return _p(num_1) + " " + p_1.name;
            }
            i_3 = i_3 + 1;
        }
        return _p(value);
    }
    public static void main(String[] args) {
        si_positive = ((Prefix[])(new Prefix[]{new Prefix("yotta", 24), new Prefix("zetta", 21), new Prefix("exa", 18), new Prefix("peta", 15), new Prefix("tera", 12), new Prefix("giga", 9), new Prefix("mega", 6), new Prefix("kilo", 3), new Prefix("hecto", 2), new Prefix("deca", 1)}));
        si_negative = ((Prefix[])(new Prefix[]{new Prefix("deci", -1), new Prefix("centi", -2), new Prefix("milli", -3), new Prefix("micro", -6), new Prefix("nano", -9), new Prefix("pico", -12), new Prefix("femto", -15), new Prefix("atto", -18), new Prefix("zepto", -21), new Prefix("yocto", -24)}));
        binary_prefixes = ((Prefix[])(new Prefix[]{new Prefix("yotta", 80), new Prefix("zetta", 70), new Prefix("exa", 60), new Prefix("peta", 50), new Prefix("tera", 40), new Prefix("giga", 30), new Prefix("mega", 20), new Prefix("kilo", 10)}));
        System.out.println(add_si_prefix(10000.0));
        System.out.println(add_si_prefix(0.005));
        System.out.println(add_binary_prefix(65536.0));
        System.out.println(add_binary_prefix(512.0));
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
