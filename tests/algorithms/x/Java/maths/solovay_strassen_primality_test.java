public class Main {
    static long seed = 1L;

    static void set_seed(long s) {
        seed = (long)(s);
    }

    static long randint(long a, long b) {
        seed = (long)(((long)(Math.floorMod(((long)(((long)((long)(seed) * 1103515245L) + 12345L))), 2147483648L))));
        return (long)((Math.floorMod(seed, ((long)((long)(b) - (long)(a)) + 1L)))) + (long)(a);
    }

    static long jacobi_symbol(long random_a, long number) {
        if ((long)(random_a) == 0L || (long)(random_a) == 1L) {
            return random_a;
        }
        random_a = Math.floorMod(random_a, number);
        long t_1 = 1L;
        while ((long)(random_a) != 0L) {
            while (Math.floorMod(random_a, 2) == 0L) {
                random_a = Math.floorDiv(((long)(random_a)), ((long)(2)));
                long r_1 = Math.floorMod(number, 8);
                if ((long)(r_1) == 3L || (long)(r_1) == 5L) {
                    t_1 = (long)(-t_1);
                }
            }
            long temp_1 = (long)(random_a);
            random_a = (long)(number);
            number = (long)(temp_1);
            if (Math.floorMod(random_a, 4) == 3L && Math.floorMod(number, 4) == 3L) {
                t_1 = (long)(-t_1);
            }
            random_a = Math.floorMod(random_a, number);
        }
        if ((long)(number) == 1L) {
            return t_1;
        }
        return 0;
    }

    static long pow_mod(long base, long exp, long mod) {
        long result = 1L;
        long b_1 = Math.floorMod(base, mod);
        long e_1 = (long)(exp);
        while ((long)(e_1) > 0L) {
            if (Math.floorMod(e_1, 2) == 1L) {
                result = Math.floorMod(((long)(result) * (long)(b_1)), mod);
            }
            b_1 = Math.floorMod(((long)(b_1) * (long)(b_1)), mod);
            e_1 = Math.floorDiv(e_1, 2);
        }
        return result;
    }

    static boolean solovay_strassen(long number, long iterations) {
        if ((long)(number) <= 1L) {
            return false;
        }
        if ((long)(number) <= 3L) {
            return true;
        }
        long i_1 = 0L;
        while ((long)(i_1) < (long)(iterations)) {
            long a_1 = (long)(randint(2L, (long)((long)(number) - 2L)));
            long x_1 = (long)(jacobi_symbol((long)(a_1), (long)(number)));
            long y_1 = (long)(pow_mod((long)(a_1), Math.floorDiv(((long)(((long)(number) - 1L))), ((long)(2))), (long)(number)));
            long mod_x_1 = Math.floorMod(x_1, number);
            if ((long)(mod_x_1) < 0L) {
                mod_x_1 = (long)((long)(mod_x_1) + (long)(number));
            }
            if ((long)(x_1) == 0L || (long)(y_1) != (long)(mod_x_1)) {
                return false;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return true;
    }

    static void main() {
        set_seed(10L);
        System.out.println(_p(solovay_strassen(13L, 5L)));
        System.out.println(_p(solovay_strassen(9L, 10L)));
        System.out.println(_p(solovay_strassen(17L, 15L)));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
