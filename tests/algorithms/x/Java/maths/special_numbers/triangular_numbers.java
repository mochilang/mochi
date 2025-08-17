public class Main {

    static long triangular_number(long position) {
        if ((long)(position) < 0L) {
            throw new RuntimeException(String.valueOf("position must be non-negative"));
        }
        return ((long)(Math.floorDiv(((long)((long)(position) * (long)(((long)(position) + 1L)))), ((long)(2)))));
    }

    static void test_triangular_number() {
        if ((long)(triangular_number(1L)) != 1L) {
            throw new RuntimeException(String.valueOf("triangular_number(1) failed"));
        }
        if ((long)(triangular_number(3L)) != 6L) {
            throw new RuntimeException(String.valueOf("triangular_number(3) failed"));
        }
    }

    static void main() {
        test_triangular_number();
        System.out.println(triangular_number(10L));
    }
    public static void main(String[] args) {
        main();
    }
}
