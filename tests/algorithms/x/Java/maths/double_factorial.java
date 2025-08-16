public class Main {

    static long double_factorial_recursive(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("double_factorial_recursive() not defined for negative values"));
        }
        if ((long)(n) <= 1L) {
            return 1;
        }
        return (long)(n) * (long)(double_factorial_recursive((long)((long)(n) - 2L)));
    }

    static long double_factorial_iterative(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("double_factorial_iterative() not defined for negative values"));
        }
        long result_1 = 1L;
        long i_1 = (long)(n);
        while ((long)(i_1) > 0L) {
            result_1 = (long)((long)(result_1) * (long)(i_1));
            i_1 = (long)((long)(i_1) - 2L);
        }
        return result_1;
    }

    static void test_double_factorial() {
        if ((long)(double_factorial_recursive(0L)) != 1L) {
            throw new RuntimeException(String.valueOf("0!! recursive failed"));
        }
        if ((long)(double_factorial_iterative(0L)) != 1L) {
            throw new RuntimeException(String.valueOf("0!! iterative failed"));
        }
        if ((long)(double_factorial_recursive(1L)) != 1L) {
            throw new RuntimeException(String.valueOf("1!! recursive failed"));
        }
        if ((long)(double_factorial_iterative(1L)) != 1L) {
            throw new RuntimeException(String.valueOf("1!! iterative failed"));
        }
        if ((long)(double_factorial_recursive(5L)) != 15L) {
            throw new RuntimeException(String.valueOf("5!! recursive failed"));
        }
        if ((long)(double_factorial_iterative(5L)) != 15L) {
            throw new RuntimeException(String.valueOf("5!! iterative failed"));
        }
        if ((long)(double_factorial_recursive(6L)) != 48L) {
            throw new RuntimeException(String.valueOf("6!! recursive failed"));
        }
        if ((long)(double_factorial_iterative(6L)) != 48L) {
            throw new RuntimeException(String.valueOf("6!! iterative failed"));
        }
        long n_1 = 0L;
        while ((long)(n_1) <= 10L) {
            if ((long)(double_factorial_recursive((long)(n_1))) != (long)(double_factorial_iterative((long)(n_1)))) {
                throw new RuntimeException(String.valueOf("double factorial mismatch"));
            }
            n_1 = (long)((long)(n_1) + 1L);
        }
    }

    static void main() {
        test_double_factorial();
        System.out.println(double_factorial_iterative(10L));
    }
    public static void main(String[] args) {
        main();
    }
}
