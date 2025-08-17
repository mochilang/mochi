public class Main {

    static long remove_digit(long num) {
        long n = (long)(num);
        if ((long)(n) < 0L) {
            n = (long)(-n);
        }
        long max_val_1 = 0L;
        long divisor_1 = 1L;
        while ((long)(divisor_1) <= (long)(n)) {
            long higher_1 = Math.floorDiv(n, ((long)(divisor_1) * 10L));
            long lower_1 = Math.floorMod(n, divisor_1);
            long candidate_1 = (long)((long)((long)(higher_1) * (long)(divisor_1)) + (long)(lower_1));
            if ((long)(candidate_1) > (long)(max_val_1)) {
                max_val_1 = (long)(candidate_1);
            }
            divisor_1 = (long)((long)(divisor_1) * 10L);
        }
        return max_val_1;
    }

    static void test_remove_digit() {
        if ((long)(remove_digit(152L)) != 52L) {
            throw new RuntimeException(String.valueOf("remove_digit(152) failed"));
        }
        if ((long)(remove_digit(6385L)) != 685L) {
            throw new RuntimeException(String.valueOf("remove_digit(6385) failed"));
        }
        if ((long)(remove_digit((long)(-11))) != 1L) {
            throw new RuntimeException(String.valueOf("remove_digit(-11) failed"));
        }
        if ((long)(remove_digit(2222222L)) != 222222L) {
            throw new RuntimeException(String.valueOf("remove_digit(2222222) failed"));
        }
    }

    static void main() {
        test_remove_digit();
        System.out.println(remove_digit(152L));
    }
    public static void main(String[] args) {
        main();
    }
}
