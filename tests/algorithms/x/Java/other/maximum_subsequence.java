public class Main {

    static long max_int(long a, long b) {
        if ((long)(a) >= (long)(b)) {
            return a;
        } else {
            return b;
        }
    }

    static long max_subsequence_sum(long[] nums) {
        if ((long)(nums.length) == 0L) {
            throw new RuntimeException(String.valueOf("input sequence should not be empty"));
        }
        long ans_1 = (long)(nums[(int)(0L)]);
        long i_1 = 1L;
        while ((long)(i_1) < (long)(nums.length)) {
            long num_1 = (long)(nums[(int)((long)(i_1))]);
            long extended_1 = (long)((long)(ans_1) + (long)(num_1));
            ans_1 = (long)(max_int((long)(max_int((long)(ans_1), (long)(extended_1))), (long)(num_1)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return ans_1;
    }
    public static void main(String[] args) {
        System.out.println(max_subsequence_sum(((long[])(new long[]{1, 2, 3, 4, -2}))));
        System.out.println(max_subsequence_sum(((long[])(new long[]{-2, -3, -1, -4, -6}))));
    }
}
