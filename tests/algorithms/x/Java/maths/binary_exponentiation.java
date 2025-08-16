public class Main {

    static double binary_exp_recursive(double base, long exponent) {
        if ((long)(exponent) < 0L) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if ((long)(exponent) == 0L) {
            return 1.0;
        }
        if (Math.floorMod(exponent, 2) == 1L) {
            return (double)(binary_exp_recursive((double)(base), (long)((long)(exponent) - 1L))) * (double)(base);
        }
        double half_1 = (double)(binary_exp_recursive((double)(base), Math.floorDiv(((long)(exponent)), ((long)(2)))));
        return (double)(half_1) * (double)(half_1);
    }

    static double binary_exp_iterative(double base, long exponent) {
        if ((long)(exponent) < 0L) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        double result_1 = (double)(1.0);
        double b_1 = (double)(base);
        long e_1 = (long)(exponent);
        while ((long)(e_1) > 0L) {
            if (Math.floorMod(e_1, 2) == 1L) {
                result_1 = (double)((double)(result_1) * (double)(b_1));
            }
            b_1 = (double)((double)(b_1) * (double)(b_1));
            e_1 = Math.floorDiv(e_1, 2);
        }
        return result_1;
    }

    static long binary_exp_mod_recursive(long base, long exponent, long modulus) {
        if ((long)(exponent) < 0L) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if ((long)(modulus) <= 0L) {
            throw new RuntimeException(String.valueOf("modulus must be positive"));
        }
        if ((long)(exponent) == 0L) {
            return Math.floorMod(1, modulus);
        }
        if (Math.floorMod(exponent, 2) == 1L) {
            return Math.floorMod(((long)(binary_exp_mod_recursive((long)(base), (long)((long)(exponent) - 1L), (long)(modulus))) * (long)((Math.floorMod(base, modulus)))), modulus);
        }
        long r_1 = (long)(binary_exp_mod_recursive((long)(base), Math.floorDiv(((long)(exponent)), ((long)(2))), (long)(modulus)));
        return Math.floorMod(((long)(r_1) * (long)(r_1)), modulus);
    }

    static long binary_exp_mod_iterative(long base, long exponent, long modulus) {
        if ((long)(exponent) < 0L) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if ((long)(modulus) <= 0L) {
            throw new RuntimeException(String.valueOf("modulus must be positive"));
        }
        long result_3 = Math.floorMod(1, modulus);
        long b_3 = Math.floorMod(base, modulus);
        long e_3 = (long)(exponent);
        while ((long)(e_3) > 0L) {
            if (Math.floorMod(e_3, 2) == 1L) {
                result_3 = Math.floorMod(((long)(result_3) * (long)(b_3)), modulus);
            }
            b_3 = Math.floorMod(((long)(b_3) * (long)(b_3)), modulus);
            e_3 = Math.floorDiv(e_3, 2);
        }
        return result_3;
    }
    public static void main(String[] args) {
        System.out.println(binary_exp_recursive((double)(3.0), 5L));
        System.out.println(binary_exp_iterative((double)(1.5), 4L));
        System.out.println(binary_exp_mod_recursive(3L, 4L, 5L));
        System.out.println(binary_exp_mod_iterative(11L, 13L, 7L));
    }
}
