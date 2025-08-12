public class Main {

    static long index_of_min(long[] xs) {
        long min_idx = 0L;
        long i_1 = 1L;
        while (i_1 < xs.length) {
            if (xs[(int)((long)(i_1))] < xs[(int)((long)(min_idx))]) {
                min_idx = i_1;
            }
            i_1 = i_1 + 1;
        }
        return min_idx;
    }

    static long[] remove_at(long[] xs, long idx) {
        long[] res = ((long[])(new long[]{}));
        long i_3 = 0L;
        while (i_3 < xs.length) {
            if (i_3 != idx) {
                res = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(res), java.util.stream.LongStream.of(xs[(int)((long)(i_3))])).toArray()));
            }
            i_3 = i_3 + 1;
        }
        return res;
    }

    static long optimal_merge_pattern(long[] files) {
        long[] arr = ((long[])(files));
        long optimal_merge_cost_1 = 0L;
        while (arr.length > 1) {
            long temp_1 = 0L;
            long k_1 = 0L;
            while (k_1 < 2) {
                long min_idx_2 = index_of_min(((long[])(arr)));
                temp_1 = temp_1 + arr[(int)((long)(min_idx_2))];
                arr = ((long[])(remove_at(((long[])(arr)), min_idx_2)));
                k_1 = k_1 + 1;
            }
            arr = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(arr), java.util.stream.LongStream.of(temp_1)).toArray()));
            optimal_merge_cost_1 = optimal_merge_cost_1 + temp_1;
        }
        return optimal_merge_cost_1;
    }
    public static void main(String[] args) {
        System.out.println(optimal_merge_pattern(((long[])(new long[]{2, 3, 4}))));
        System.out.println(optimal_merge_pattern(((long[])(new long[]{5, 10, 20, 30, 30}))));
        System.out.println(optimal_merge_pattern(((long[])(new long[]{8, 8, 8, 8, 8}))));
    }
}
