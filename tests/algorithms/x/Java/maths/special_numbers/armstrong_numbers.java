public class Main {

    static long pow_int(long base, long exp) {
        long result = 1L;
        long i_1 = 0L;
        while ((long)(i_1) < (long)(exp)) {
            result = (long)((long)(result) * (long)(base));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return result;
    }

    static boolean armstrong_number(long n) {
        if ((long)(n) < 1L) {
            return false;
        }
        long digits_1 = 0L;
        long temp_1 = (long)(n);
        while ((long)(temp_1) > 0L) {
            temp_1 = Math.floorDiv(temp_1, 10);
            digits_1 = (long)((long)(digits_1) + 1L);
        }
        long total_1 = 0L;
        temp_1 = (long)(n);
        while ((long)(temp_1) > 0L) {
            long rem_1 = Math.floorMod(temp_1, 10);
            total_1 = (long)((long)(total_1) + (long)(pow_int((long)(rem_1), (long)(digits_1))));
            temp_1 = Math.floorDiv(temp_1, 10);
        }
        return (long)(total_1) == (long)(n);
    }

    static boolean pluperfect_number(long n) {
        if ((long)(n) < 1L) {
            return false;
        }
        long[] digit_histogram_1 = ((long[])(new long[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < 10L) {
            digit_histogram_1 = ((long[])(appendLong(digit_histogram_1, 0L)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        long digit_total_1 = 0L;
        long temp_3 = (long)(n);
        while ((long)(temp_3) > 0L) {
            long rem_3 = Math.floorMod(temp_3, 10);
digit_histogram_1[(int)((long)(rem_3))] = (long)((long)(digit_histogram_1[(int)((long)(rem_3))]) + 1L);
            digit_total_1 = (long)((long)(digit_total_1) + 1L);
            temp_3 = Math.floorDiv(temp_3, 10);
        }
        long total_3 = 0L;
        i_3 = 0L;
        while ((long)(i_3) < 10L) {
            if ((long)(digit_histogram_1[(int)((long)(i_3))]) > 0L) {
                total_3 = (long)((long)(total_3) + (long)((long)(digit_histogram_1[(int)((long)(i_3))]) * (long)(pow_int((long)(i_3), (long)(digit_total_1)))));
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (long)(total_3) == (long)(n);
    }

    static boolean narcissistic_number(long n) {
        if ((long)(n) < 1L) {
            return false;
        }
        long digits_3 = 0L;
        long temp_5 = (long)(n);
        while ((long)(temp_5) > 0L) {
            temp_5 = Math.floorDiv(temp_5, 10);
            digits_3 = (long)((long)(digits_3) + 1L);
        }
        temp_5 = (long)(n);
        long total_5 = 0L;
        while ((long)(temp_5) > 0L) {
            long rem_5 = Math.floorMod(temp_5, 10);
            total_5 = (long)((long)(total_5) + (long)(pow_int((long)(rem_5), (long)(digits_3))));
            temp_5 = Math.floorDiv(temp_5, 10);
        }
        return (long)(total_5) == (long)(n);
    }
    public static void main(String[] args) {
        System.out.println(armstrong_number(371L));
        System.out.println(armstrong_number(200L));
        System.out.println(pluperfect_number(371L));
        System.out.println(pluperfect_number(200L));
        System.out.println(narcissistic_number(371L));
        System.out.println(narcissistic_number(200L));
    }

    static long[] appendLong(long[] arr, long v) {
        long[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
