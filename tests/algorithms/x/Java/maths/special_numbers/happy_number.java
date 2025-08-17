public class Main {

    static boolean is_happy_number(long num) {
        if ((long)(num) <= 0L) {
            throw new RuntimeException(String.valueOf("num must be a positive integer"));
        }
        long[] seen_1 = ((long[])(new long[]{}));
        long n_1 = (long)(num);
        while ((long)(n_1) != 1L) {
            long i_1 = 0L;
            while ((long)(i_1) < (long)(seen_1.length)) {
                if ((long)(seen_1[(int)((long)(i_1))]) == (long)(n_1)) {
                    return false;
                }
                i_1 = (long)((long)(i_1) + 1L);
            }
            seen_1 = ((long[])(appendLong(seen_1, (long)(n_1))));
            long total_1 = 0L;
            long temp_1 = (long)(n_1);
            while ((long)(temp_1) > 0L) {
                long digit_1 = Math.floorMod(temp_1, 10);
                total_1 = (long)((long)(total_1) + (long)((long)(digit_1) * (long)(digit_1)));
                temp_1 = Math.floorDiv(temp_1, 10);
            }
            n_1 = (long)(total_1);
        }
        return true;
    }

    static void test_is_happy_number() {
        if (!(Boolean)is_happy_number(19L)) {
            throw new RuntimeException(String.valueOf("19 should be happy"));
        }
        if (is_happy_number(2L)) {
            throw new RuntimeException(String.valueOf("2 should be unhappy"));
        }
        if (!(Boolean)is_happy_number(23L)) {
            throw new RuntimeException(String.valueOf("23 should be happy"));
        }
        if (!(Boolean)is_happy_number(1L)) {
            throw new RuntimeException(String.valueOf("1 should be happy"));
        }
    }

    static void main() {
        test_is_happy_number();
        System.out.println(is_happy_number(19L));
    }
    public static void main(String[] args) {
        main();
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
