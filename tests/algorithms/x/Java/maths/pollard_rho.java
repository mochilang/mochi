public class Main {
    static class PollardResult {
        long factor;
        boolean ok;
        PollardResult(long factor, boolean ok) {
            this.factor = factor;
            this.ok = ok;
        }
        PollardResult() {}
        @Override public String toString() {
            return String.format("{'factor': %s, 'ok': %s}", String.valueOf(factor), String.valueOf(ok));
        }
    }


    static long gcd(long a, long b) {
        long x = (long)((long)(a) < 0L ? -a : a);
        long y_1 = (long)((long)(b) < 0L ? -b : b);
        while ((long)(y_1) != 0L) {
            long t_1 = Math.floorMod(x, y_1);
            x = (long)(y_1);
            y_1 = (long)(t_1);
        }
        return x;
    }

    static long rand_fn(long value, long step, long modulus) {
        return Math.floorMod(((long)((long)(value) * (long)(value)) + (long)(step)), modulus);
    }

    static PollardResult pollard_rho(long num, long seed, long step, long attempts) {
        if ((long)(num) < 2L) {
            throw new RuntimeException(String.valueOf("The input value cannot be less than 2"));
        }
        if ((long)(num) > 2L && Math.floorMod(num, 2) == 0L) {
            return new PollardResult(2, true);
        }
        long s_1 = (long)(seed);
        long st_1 = (long)(step);
        long i_1 = 0L;
        while ((long)(i_1) < (long)(attempts)) {
            long tortoise_1 = (long)(s_1);
            long hare_1 = (long)(s_1);
            while (true) {
                tortoise_1 = (long)(rand_fn((long)(tortoise_1), (long)(st_1), (long)(num)));
                hare_1 = (long)(rand_fn((long)(hare_1), (long)(st_1), (long)(num)));
                hare_1 = (long)(rand_fn((long)(hare_1), (long)(st_1), (long)(num)));
                long divisor_1 = (long)(gcd((long)((long)(hare_1) - (long)(tortoise_1)), (long)(num)));
                if ((long)(divisor_1) == 1L) {
                    continue;
                } else                 if ((long)(divisor_1) == (long)(num)) {
                    break;
                } else {
                    return new PollardResult(divisor_1, true);
                }
            }
            s_1 = (long)(hare_1);
            st_1 = (long)((long)(st_1) + 1L);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return new PollardResult(0, false);
    }

    static void test_pollard_rho() {
        PollardResult r1 = pollard_rho(8051L, 2L, 1L, 5L);
        if (!r1.ok || ((long)(r1.factor) != 83L && (long)(r1.factor) != 97L)) {
            throw new RuntimeException(String.valueOf("test1 failed"));
        }
        PollardResult r2_1 = pollard_rho(10403L, 2L, 1L, 5L);
        if (!r2_1.ok || ((long)(r2_1.factor) != 101L && (long)(r2_1.factor) != 103L)) {
            throw new RuntimeException(String.valueOf("test2 failed"));
        }
        PollardResult r3_1 = pollard_rho(100L, 2L, 1L, 3L);
        if (!r3_1.ok || (long)(r3_1.factor) != 2L) {
            throw new RuntimeException(String.valueOf("test3 failed"));
        }
        PollardResult r4_1 = pollard_rho(17L, 2L, 1L, 3L);
        if (r4_1.ok) {
            throw new RuntimeException(String.valueOf("test4 failed"));
        }
        PollardResult r5_1 = pollard_rho((long)((long)(17L * 17L) * 17L), 2L, 1L, 3L);
        if (!r5_1.ok || (long)(r5_1.factor) != 17L) {
            throw new RuntimeException(String.valueOf("test5 failed"));
        }
        PollardResult r6_1 = pollard_rho((long)((long)(17L * 17L) * 17L), 2L, 1L, 1L);
        if (r6_1.ok) {
            throw new RuntimeException(String.valueOf("test6 failed"));
        }
        PollardResult r7_1 = pollard_rho((long)((long)(3L * 5L) * 7L), 2L, 1L, 3L);
        if (!r7_1.ok || (long)(r7_1.factor) != 21L) {
            throw new RuntimeException(String.valueOf("test7 failed"));
        }
    }

    static void main() {
        test_pollard_rho();
        PollardResult a_1 = pollard_rho(100L, 2L, 1L, 3L);
        if (a_1.ok) {
            System.out.println(_p(a_1.factor));
        } else {
            System.out.println("None");
        }
        PollardResult b_1 = pollard_rho(17L, 2L, 1L, 3L);
        if (b_1.ok) {
            System.out.println(_p(b_1.factor));
        } else {
            System.out.println("None");
        }
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
