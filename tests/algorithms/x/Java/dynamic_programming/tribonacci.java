public class Main {

    static long[] tribonacci(long num) {
        long[] dp = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(num)) {
            if ((long)(i_1) == (long)(0) || (long)(i_1) == (long)(1)) {
                dp = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(dp), java.util.stream.LongStream.of(0L)).toArray()));
            } else             if ((long)(i_1) == (long)(2)) {
                dp = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(dp), java.util.stream.LongStream.of(1L)).toArray()));
            } else {
                long t_1 = (long)((long)((long)(dp[(int)((long)((long)(i_1) - (long)(1)))]) + (long)(dp[(int)((long)((long)(i_1) - (long)(2)))])) + (long)(dp[(int)((long)((long)(i_1) - (long)(3)))]));
                dp = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(dp), java.util.stream.LongStream.of((long)(t_1))).toArray()));
            }
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return dp;
    }
    public static void main(String[] args) {
        System.out.println(tribonacci(8L));
    }
}
