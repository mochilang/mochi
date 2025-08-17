public class Main {

    static long[] hamming(long n) {
        if ((long)(n) < 1L) {
            throw new RuntimeException(String.valueOf("n_element should be a positive number"));
        }
        long[] hamming_list_1 = ((long[])(new long[]{1}));
        long i_1 = 0L;
        long j_1 = 0L;
        long k_1 = 0L;
        long index_1 = 1L;
        while ((long)(index_1) < (long)(n)) {
            while ((long)((long)(hamming_list_1[(int)((long)(i_1))]) * 2L) <= (long)(hamming_list_1[(int)((long)((long)(hamming_list_1.length) - 1L))])) {
                i_1 = (long)((long)(i_1) + 1L);
            }
            while ((long)((long)(hamming_list_1[(int)((long)(j_1))]) * 3L) <= (long)(hamming_list_1[(int)((long)((long)(hamming_list_1.length) - 1L))])) {
                j_1 = (long)((long)(j_1) + 1L);
            }
            while ((long)((long)(hamming_list_1[(int)((long)(k_1))]) * 5L) <= (long)(hamming_list_1[(int)((long)((long)(hamming_list_1.length) - 1L))])) {
                k_1 = (long)((long)(k_1) + 1L);
            }
            long m1_1 = (long)((long)(hamming_list_1[(int)((long)(i_1))]) * 2L);
            long m2_1 = (long)((long)(hamming_list_1[(int)((long)(j_1))]) * 3L);
            long m3_1 = (long)((long)(hamming_list_1[(int)((long)(k_1))]) * 5L);
            long next_1 = (long)(m1_1);
            if ((long)(m2_1) < (long)(next_1)) {
                next_1 = (long)(m2_1);
            }
            if ((long)(m3_1) < (long)(next_1)) {
                next_1 = (long)(m3_1);
            }
            hamming_list_1 = ((long[])(appendLong(hamming_list_1, (long)(next_1))));
            index_1 = (long)((long)(index_1) + 1L);
        }
        return hamming_list_1;
    }
    public static void main(String[] args) {
        System.out.println(hamming(5L));
        System.out.println(hamming(10L));
        System.out.println(hamming(15L));
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
