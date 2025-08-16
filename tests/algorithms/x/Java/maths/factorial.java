public class Main {

    static long factorial(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("factorial() not defined for negative values"));
        }
        long value_1 = 1L;
        long i_1 = 1L;
        while ((long)(i_1) <= (long)(n)) {
            value_1 = (long)((long)(value_1) * (long)(i_1));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return value_1;
    }

    static long factorial_recursive(long n) {
        if ((long)(n) < 0L) {
            throw new RuntimeException(String.valueOf("factorial() not defined for negative values"));
        }
        if ((long)(n) <= 1L) {
            return 1;
        }
        return (long)(n) * (long)(factorial_recursive((long)((long)(n) - 1L)));
    }

    static void test_factorial() {
        long i_2 = 0L;
        while ((long)(i_2) <= 10L) {
            if ((long)(factorial((long)(i_2))) != (long)(factorial_recursive((long)(i_2)))) {
                throw new RuntimeException(String.valueOf("mismatch between factorial and factorial_recursive"));
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        if ((long)(factorial(6L)) != 720L) {
            throw new RuntimeException(String.valueOf("factorial(6) should be 720"));
        }
    }

    static void main() {
        test_factorial();
        System.out.println(factorial(6L));
    }
    public static void main(String[] args) {
        main();
    }
}
