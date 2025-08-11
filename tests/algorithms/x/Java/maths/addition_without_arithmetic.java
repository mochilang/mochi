public class Main {
    static long MAX;
    static long HALF;

    static long to_unsigned(long n) {
        if (n < 0) {
            return MAX + n;
        }
        return n;
    }

    static long from_unsigned(long n) {
        if (n >= HALF) {
            return n - MAX;
        }
        return n;
    }

    static long bit_and(long a, long b) {
        long x = a;
        long y_1 = b;
        long res_1 = 0;
        long bit_1 = 1;
        long i_1 = 0;
        while (i_1 < 32) {
            if ((Math.floorMod(x, 2) == 1) && (Math.floorMod(y_1, 2) == 1)) {
                res_1 = res_1 + bit_1;
            }
            x = Math.floorDiv(x, 2);
            y_1 = Math.floorDiv(y_1, 2);
            bit_1 = bit_1 * 2;
            i_1 = i_1 + 1;
        }
        return res_1;
    }

    static long bit_xor(long a, long b) {
        long x_1 = a;
        long y_3 = b;
        long res_3 = 0;
        long bit_3 = 1;
        long i_3 = 0;
        while (i_3 < 32) {
            long abit_1 = Math.floorMod(x_1, 2);
            long bbit_1 = Math.floorMod(y_3, 2);
            if (Math.floorMod((abit_1 + bbit_1), 2) == 1) {
                res_3 = res_3 + bit_3;
            }
            x_1 = Math.floorDiv(x_1, 2);
            y_3 = Math.floorDiv(y_3, 2);
            bit_3 = bit_3 * 2;
            i_3 = i_3 + 1;
        }
        return res_3;
    }

    static long lshift1(long num) {
        return Math.floorMod((num * 2), MAX);
    }

    static long add(long a, long b) {
        long first = to_unsigned(a);
        long second_1 = to_unsigned(b);
        while (second_1 != 0) {
            long carry_1 = bit_and(first, second_1);
            first = bit_xor(first, second_1);
            second_1 = lshift1(carry_1);
        }
        long result_1 = from_unsigned(first);
        return result_1;
    }
    public static void main(String[] args) {
        MAX = 4294967296L;
        HALF = 2147483648L;
        System.out.println(_p(add(3, 5)));
        System.out.println(_p(add(13, 5)));
        System.out.println(_p(add(-7, 2)));
        System.out.println(_p(add(0, -7)));
        System.out.println(_p(add(-321, 0)));
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
