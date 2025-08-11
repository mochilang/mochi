public class Main {

    static boolean contains_int(long[] xs, long x) {
        long i = 0;
        while (i < xs.length) {
            if (xs[(int)(i)] == x) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static boolean contains_string(String[] xs, String x) {
        long i_1 = 0;
        while (i_1 < xs.length) {
            if ((xs[(int)(i_1)].equals(x))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static long count_int(long[] xs, long x) {
        long cnt = 0;
        long i_3 = 0;
        while (i_3 < xs.length) {
            if (xs[(int)(i_3)] == x) {
                cnt = cnt + 1;
            }
            i_3 = i_3 + 1;
        }
        return cnt;
    }

    static long count_string(String[] xs, String x) {
        long cnt_1 = 0;
        long i_5 = 0;
        while (i_5 < xs.length) {
            if ((xs[(int)(i_5)].equals(x))) {
                cnt_1 = cnt_1 + 1;
            }
            i_5 = i_5 + 1;
        }
        return cnt_1;
    }

    static long[] sort_int(long[] xs) {
        long[] arr = ((long[])(xs));
        long i_7 = 0;
        while (i_7 < arr.length) {
            long j_1 = i_7 + 1;
            while (j_1 < arr.length) {
                if (arr[(int)(j_1)] < arr[(int)(i_7)]) {
                    long tmp_1 = arr[(int)(i_7)];
arr[(int)(i_7)] = arr[(int)(j_1)];
arr[(int)(j_1)] = tmp_1;
                }
                j_1 = j_1 + 1;
            }
            i_7 = i_7 + 1;
        }
        return arr;
    }

    static String[] sort_string(String[] xs) {
        String[] arr_1 = ((String[])(xs));
        long i_9 = 0;
        while (i_9 < arr_1.length) {
            long j_3 = i_9 + 1;
            while (j_3 < arr_1.length) {
                if ((arr_1[(int)(j_3)].compareTo(arr_1[(int)(i_9)]) < 0)) {
                    String tmp_3 = arr_1[(int)(i_9)];
arr_1[(int)(i_9)] = arr_1[(int)(j_3)];
arr_1[(int)(j_3)] = tmp_3;
                }
                j_3 = j_3 + 1;
            }
            i_9 = i_9 + 1;
        }
        return arr_1;
    }

    static long[] mode_int(long[] lst) {
        if (lst.length == 0) {
            return new long[]{};
        }
        long[] counts_1 = ((long[])(new long[]{}));
        long i_11 = 0;
        while (i_11 < lst.length) {
            counts_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(counts_1), java.util.stream.LongStream.of(count_int(((long[])(lst)), lst[(int)(i_11)]))).toArray()));
            i_11 = i_11 + 1;
        }
        long max_count_1 = 0;
        i_11 = 0;
        while (i_11 < counts_1.length) {
            if (counts_1[(int)(i_11)] > max_count_1) {
                max_count_1 = counts_1[(int)(i_11)];
            }
            i_11 = i_11 + 1;
        }
        long[] modes_1 = ((long[])(new long[]{}));
        i_11 = 0;
        while (i_11 < lst.length) {
            if (counts_1[(int)(i_11)] == max_count_1) {
                long v_1 = lst[(int)(i_11)];
                if (!(Boolean)contains_int(((long[])(modes_1)), v_1)) {
                    modes_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(modes_1), java.util.stream.LongStream.of(v_1)).toArray()));
                }
            }
            i_11 = i_11 + 1;
        }
        return sort_int(((long[])(modes_1)));
    }

    static String[] mode_string(String[] lst) {
        if (lst.length == 0) {
            return new String[]{};
        }
        long[] counts_3 = ((long[])(new long[]{}));
        long i_13 = 0;
        while (i_13 < lst.length) {
            counts_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(counts_3), java.util.stream.LongStream.of(count_string(((String[])(lst)), lst[(int)(i_13)]))).toArray()));
            i_13 = i_13 + 1;
        }
        long max_count_3 = 0;
        i_13 = 0;
        while (i_13 < counts_3.length) {
            if (counts_3[(int)(i_13)] > max_count_3) {
                max_count_3 = counts_3[(int)(i_13)];
            }
            i_13 = i_13 + 1;
        }
        String[] modes_3 = ((String[])(new String[]{}));
        i_13 = 0;
        while (i_13 < lst.length) {
            if (counts_3[(int)(i_13)] == max_count_3) {
                String v_3 = lst[(int)(i_13)];
                if (!(Boolean)contains_string(((String[])(modes_3)), v_3)) {
                    modes_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(modes_3), java.util.stream.Stream.of(v_3)).toArray(String[]::new)));
                }
            }
            i_13 = i_13 + 1;
        }
        return sort_string(((String[])(modes_3)));
    }
    public static void main(String[] args) {
        System.out.println(mode_int(((long[])(new long[]{2, 3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 2, 2, 2}))));
        System.out.println(mode_int(((long[])(new long[]{3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 2, 2, 2}))));
        System.out.println(mode_int(((long[])(new long[]{3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 4, 2, 2, 4, 2}))));
        System.out.println(mode_string(((String[])(new String[]{"x", "y", "y", "z"}))));
        System.out.println(mode_string(((String[])(new String[]{"x", "x", "y", "y", "z"}))));
    }
}
