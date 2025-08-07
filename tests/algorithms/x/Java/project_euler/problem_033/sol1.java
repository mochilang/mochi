public class Main {
    static class FractionPair {
        int num;
        int den;
        FractionPair(int num, int den) {
            this.num = num;
            this.den = den;
        }
        FractionPair() {}
        @Override public String toString() {
            return String.format("{'num': %s, 'den': %s}", String.valueOf(num), String.valueOf(den));
        }
    }


    static int gcd(int a, int b) {
        int x = a < 0 ? -a : a;
        int y = b < 0 ? -b : b;
        while (y != 0) {
            int t = Math.floorMod(x, y);
            x = y;
            y = t;
        }
        return x;
    }

    static boolean is_digit_cancelling(int num, int den) {
        if (num >= den) {
            return false;
        }
        int num_unit = Math.floorMod(num, 10);
        int num_tens = Math.floorDiv(num, 10);
        int den_unit = Math.floorMod(den, 10);
        int den_tens = Math.floorDiv(den, 10);
        if (num_unit != den_tens) {
            return false;
        }
        if (den_unit == 0) {
            return false;
        }
        return num * den_unit == num_tens * den;
    }

    static FractionPair[] find_fractions() {
        FractionPair[] sols = ((FractionPair[])(new FractionPair[]{}));
        int num = 10;
        while (num < 100) {
            int den = num + 1;
            while (den < 100) {
                if (((Boolean)(is_digit_cancelling(num, den)))) {
                    sols = ((FractionPair[])(java.util.stream.Stream.concat(java.util.Arrays.stream(sols), java.util.stream.Stream.of(new FractionPair(num, den))).toArray(FractionPair[]::new)));
                }
                den = den + 1;
            }
            num = num + 1;
        }
        return sols;
    }

    static int solution() {
        FractionPair[] fracs = ((FractionPair[])(find_fractions()));
        int num_prod = 1;
        int den_prod = 1;
        int i = 0;
        while (i < fracs.length) {
            FractionPair f = fracs[i];
            num_prod = num_prod * f.num;
            den_prod = den_prod * f.den;
            i = i + 1;
        }
        int g = gcd(num_prod, den_prod);
        return Math.floorDiv(den_prod, g);
    }

    static void main() {
        System.out.println(_p(solution()));
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
        return String.valueOf(v);
    }
}
