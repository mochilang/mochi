public class Main {
    static long p = 701L;
    static long a = 1000000000L;
    static long b_2 = 10L;
    static long left;
    static long right_fast;
    static long right_naive;

    static long binary_exponentiation(long a, long n, long mod) {
        if ((long)(n) == 0L) {
            return 1;
        }
        if (Math.floorMod(n, 2) == 1L) {
            return Math.floorMod(((long)(binary_exponentiation((long)(a), (long)((long)(n) - 1L), (long)(mod))) * (long)(a)), mod);
        }
        long b_1 = (long)(binary_exponentiation((long)(a), Math.floorDiv(((long)(n)), ((long)(2))), (long)(mod)));
        return Math.floorMod(((long)(b_1) * (long)(b_1)), mod);
    }

    static long naive_exponent_mod(long a, long n, long mod) {
        long result = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            result = Math.floorMod(((long)(result) * (long)(a)), mod);
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static void print_bool(boolean b) {
        if (b) {
            System.out.println(true ? "True" : "False");
        } else {
            System.out.println(false ? "True" : "False");
        }
    }
    public static void main(String[] args) {
        left = Math.floorMod((Math.floorDiv(a, b_2)), p);
        right_fast = Math.floorMod(((long)(a) * (long)(binary_exponentiation((long)(b_2), (long)((long)(p) - 2L), (long)(p)))), p);
        print_bool((long)(left) == (long)(right_fast));
        right_naive = Math.floorMod(((long)(a) * (long)(naive_exponent_mod((long)(b_2), (long)((long)(p) - 2L), (long)(p)))), p);
        print_bool((long)(left) == (long)(right_naive));
    }
}
