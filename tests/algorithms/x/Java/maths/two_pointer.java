public class Main {

    static long[] two_pointer(long[] nums, long target) {
        long i = 0L;
        long j_1 = (long)((long)(nums.length) - (long)(1));
        while ((long)(i) < (long)(j_1)) {
            long s_1 = (long)(nums[(int)((long)(i))] + nums[(int)((long)(j_1))]);
            if ((long)(s_1) == target) {
                return new long[]{i, j_1};
            }
            if ((long)(s_1) < target) {
                i = (long)((long)(i) + (long)(1));
            } else {
                j_1 = (long)((long)(j_1) - (long)(1));
            }
        }
        return new long[]{};
    }

    static void test_two_pointer() {
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 9L), new long[]{0, 1})) {
            throw new RuntimeException(String.valueOf("case1"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 17L), new long[]{0, 3})) {
            throw new RuntimeException(String.valueOf("case2"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 18L), new long[]{1, 2})) {
            throw new RuntimeException(String.valueOf("case3"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 26L), new long[]{2, 3})) {
            throw new RuntimeException(String.valueOf("case4"));
        }
        if (!java.util.Arrays.equals(two_pointer(((long[])(new long[]{1, 3, 3})), 6L), new long[]{1, 2})) {
            throw new RuntimeException(String.valueOf("case5"));
        }
        if ((long)(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 8L).length) != (long)(0)) {
            throw new RuntimeException(String.valueOf("case6"));
        }
        if ((long)(two_pointer(((long[])(new long[]{0, 3, 6, 9, 12, 15, 18, 21, 24, 27})), 19L).length) != (long)(0)) {
            throw new RuntimeException(String.valueOf("case7"));
        }
        if ((long)(two_pointer(((long[])(new long[]{1, 2, 3})), 6L).length) != (long)(0)) {
            throw new RuntimeException(String.valueOf("case8"));
        }
    }

    static void main() {
        test_two_pointer();
        System.out.println(two_pointer(((long[])(new long[]{2, 7, 11, 15})), 9L));
    }
    public static void main(String[] args) {
        main();
    }
}
