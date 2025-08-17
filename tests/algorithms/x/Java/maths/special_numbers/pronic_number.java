public class Main {

    static long int_sqrt(long n) {
        long r = 0L;
        while ((long)((long)(((long)(r) + 1L)) * (long)(((long)(r) + 1L))) <= (long)(n)) {
            r = (long)((long)(r) + 1L);
        }
        return r;
    }

    static boolean is_pronic(long n) {
        if ((long)(n) < 0L) {
            return false;
        }
        if (Math.floorMod(n, 2) != 0L) {
            return false;
        }
        long root_1 = (long)(int_sqrt((long)(n)));
        return (long)(n) == (long)((long)(root_1) * (long)(((long)(root_1) + 1L)));
    }

    static void test_is_pronic() {
        if (is_pronic((long)(-1))) {
            throw new RuntimeException(String.valueOf("-1 should not be pronic"));
        }
        if (!(Boolean)is_pronic(0L)) {
            throw new RuntimeException(String.valueOf("0 should be pronic"));
        }
        if (!(Boolean)is_pronic(2L)) {
            throw new RuntimeException(String.valueOf("2 should be pronic"));
        }
        if (is_pronic(5L)) {
            throw new RuntimeException(String.valueOf("5 should not be pronic"));
        }
        if (!(Boolean)is_pronic(6L)) {
            throw new RuntimeException(String.valueOf("6 should be pronic"));
        }
        if (is_pronic(8L)) {
            throw new RuntimeException(String.valueOf("8 should not be pronic"));
        }
        if (!(Boolean)is_pronic(30L)) {
            throw new RuntimeException(String.valueOf("30 should be pronic"));
        }
        if (is_pronic(32L)) {
            throw new RuntimeException(String.valueOf("32 should not be pronic"));
        }
        if (!(Boolean)is_pronic(2147441940L)) {
            throw new RuntimeException(String.valueOf("2147441940 should be pronic"));
        }
    }

    static void main() {
        test_is_pronic();
        System.out.println(is_pronic(56L));
    }
    public static void main(String[] args) {
        main();
    }
}
