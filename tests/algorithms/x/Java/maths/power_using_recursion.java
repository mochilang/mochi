public class Main {

    static long power(long base, long exponent) {
        if ((long)(exponent) == 0L) {
            return 1;
        }
        return (long)(base) * (long)(power((long)(base), (long)((long)(exponent) - 1L)));
    }

    static void test_power() {
        if ((long)(power(3L, 4L)) != 81L) {
            throw new RuntimeException(String.valueOf("power(3,4) failed"));
        }
        if ((long)(power(2L, 0L)) != 1L) {
            throw new RuntimeException(String.valueOf("power(2,0) failed"));
        }
        if ((long)(power(5L, 6L)) != 15625L) {
            throw new RuntimeException(String.valueOf("power(5,6) failed"));
        }
    }

    static void main() {
        test_power();
        System.out.println(power(3L, 4L));
    }
    public static void main(String[] args) {
        main();
    }
}
