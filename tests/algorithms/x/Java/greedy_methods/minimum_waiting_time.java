public class Main {

    static long[] insertion_sort(long[] a) {
        long i = 1L;
        while (i < a.length) {
            long key_1 = a[(int)((long)(i))];
            long j_1 = i - 1;
            while (j_1 >= 0 && a[(int)((long)(j_1))] > key_1) {
a[(int)((long)(j_1 + 1))] = a[(int)((long)(j_1))];
                j_1 = j_1 - 1;
            }
a[(int)((long)(j_1 + 1))] = key_1;
            i = i + 1;
        }
        return a;
    }

    static long minimum_waiting_time(long[] queries) {
        long n = queries.length;
        if (n == 0 || n == 1) {
            return 0;
        }
        long[] sorted_1 = ((long[])(insertion_sort(((long[])(queries)))));
        long total_1 = 0L;
        long i_2 = 0L;
        while (i_2 < n) {
            total_1 = total_1 + sorted_1[(int)((long)(i_2))] * (n - i_2 - 1);
            i_2 = i_2 + 1;
        }
        return total_1;
    }
    public static void main(String[] args) {
        System.out.println(minimum_waiting_time(((long[])(new long[]{3, 2, 1, 2, 6}))));
        System.out.println(minimum_waiting_time(((long[])(new long[]{3, 2, 1}))));
        System.out.println(minimum_waiting_time(((long[])(new long[]{1, 2, 3, 4}))));
        System.out.println(minimum_waiting_time(((long[])(new long[]{5, 5, 5, 5}))));
        System.out.println(minimum_waiting_time(((long[])(new long[]{}))));
    }
}
