public class Main {

    static double binary_exp_recursive(double base, long exponent) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if (exponent == 0) {
            return 1.0;
        }
        if (Math.floorMod(exponent, 2) == 1) {
            return binary_exp_recursive(base, exponent - 1) * base;
        }
        double half_1 = binary_exp_recursive(base, Math.floorDiv(exponent, 2));
        return half_1 * half_1;
    }

    static double binary_exp_iterative(double base, long exponent) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        double result_1 = 1.0;
        double b_1 = base;
        long e_1 = exponent;
        while (e_1 > 0) {
            if (Math.floorMod(e_1, 2) == 1) {
                result_1 = result_1 * b_1;
            }
            b_1 = b_1 * b_1;
            e_1 = Math.floorDiv(e_1, 2);
        }
        return result_1;
    }

    static long binary_exp_mod_recursive(long base, long exponent, long modulus) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if (modulus <= 0) {
            throw new RuntimeException(String.valueOf("modulus must be positive"));
        }
        if (exponent == 0) {
            return Math.floorMod(1, modulus);
        }
        if (Math.floorMod(exponent, 2) == 1) {
            return Math.floorMod((binary_exp_mod_recursive(base, exponent - 1, modulus) * (Math.floorMod(base, modulus))), modulus);
        }
        long r_1 = binary_exp_mod_recursive(base, Math.floorDiv(exponent, 2), modulus);
        return Math.floorMod((r_1 * r_1), modulus);
    }

    static long binary_exp_mod_iterative(long base, long exponent, long modulus) {
        if (exponent < 0) {
            throw new RuntimeException(String.valueOf("exponent must be non-negative"));
        }
        if (modulus <= 0) {
            throw new RuntimeException(String.valueOf("modulus must be positive"));
        }
        long result_3 = Math.floorMod(1, modulus);
        long b_3 = Math.floorMod(base, modulus);
        long e_3 = exponent;
        while (e_3 > 0) {
            if (Math.floorMod(e_3, 2) == 1) {
                result_3 = Math.floorMod((result_3 * b_3), modulus);
            }
            b_3 = Math.floorMod((b_3 * b_3), modulus);
            e_3 = Math.floorDiv(e_3, 2);
        }
        return result_3;
    }
    public static void main(String[] args) {
        System.out.println(binary_exp_recursive(3.0, 5));
        System.out.println(binary_exp_iterative(1.5, 4));
        System.out.println(binary_exp_mod_recursive(3, 4, 5));
        System.out.println(binary_exp_mod_iterative(11, 13, 7));
    }
}
