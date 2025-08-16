public class Main {

    static boolean contains_int(long[] xs, long x) {
        long i = 0L;
        while ((long)(i) < (long)(xs.length)) {
            if ((long)(xs[(int)((long)(i))]) == (long)(x)) {
                return true;
            }
            i = (long)((long)(i) + 1L);
        }
        return false;
    }

    static boolean contains_string(String[] xs, String x) {
        long i_1 = 0L;
        while ((long)(i_1) < (long)(xs.length)) {
            if ((xs[(int)((long)(i_1))].equals(x))) {
                return true;
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
        return false;
    }

    static long count_int(long[] xs, long x) {
        long cnt = 0L;
        long i_3 = 0L;
        while ((long)(i_3) < (long)(xs.length)) {
            if ((long)(xs[(int)((long)(i_3))]) == (long)(x)) {
                cnt = (long)((long)(cnt) + 1L);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return cnt;
    }

    static long count_string(String[] xs, String x) {
        long cnt_1 = 0L;
        long i_5 = 0L;
        while ((long)(i_5) < (long)(xs.length)) {
            if ((xs[(int)((long)(i_5))].equals(x))) {
                cnt_1 = (long)((long)(cnt_1) + 1L);
            }
            i_5 = (long)((long)(i_5) + 1L);
        }
        return cnt_1;
    }

    static long[] sort_int(long[] xs) {
        long[] arr = ((long[])(xs));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(arr.length)) {
            long j_1 = (long)((long)(i_7) + 1L);
            while ((long)(j_1) < (long)(arr.length)) {
                if ((long)(arr[(int)((long)(j_1))]) < (long)(arr[(int)((long)(i_7))])) {
                    long tmp_1 = (long)(arr[(int)((long)(i_7))]);
arr[(int)((long)(i_7))] = (long)(arr[(int)((long)(j_1))]);
arr[(int)((long)(j_1))] = (long)(tmp_1);
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            i_7 = (long)((long)(i_7) + 1L);
        }
        return arr;
    }

    static String[] sort_string(String[] xs) {
        String[] arr_1 = ((String[])(xs));
        long i_9 = 0L;
        while ((long)(i_9) < (long)(arr_1.length)) {
            long j_3 = (long)((long)(i_9) + 1L);
            while ((long)(j_3) < (long)(arr_1.length)) {
                if ((arr_1[(int)((long)(j_3))].compareTo(arr_1[(int)((long)(i_9))]) < 0)) {
                    String tmp_3 = arr_1[(int)((long)(i_9))];
arr_1[(int)((long)(i_9))] = arr_1[(int)((long)(j_3))];
arr_1[(int)((long)(j_3))] = tmp_3;
                }
                j_3 = (long)((long)(j_3) + 1L);
            }
            i_9 = (long)((long)(i_9) + 1L);
        }
        return arr_1;
    }

    static long[] mode_int(long[] lst) {
        if ((long)(lst.length) == 0L) {
            return new long[]{};
        }
        long[] counts_1 = ((long[])(new long[]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(lst.length)) {
            counts_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(counts_1), java.util.stream.LongStream.of((long)(count_int(((long[])(lst)), (long)(lst[(int)((long)(i_11))]))))).toArray()));
            i_11 = (long)((long)(i_11) + 1L);
        }
        long max_count_1 = 0L;
        i_11 = 0L;
        while ((long)(i_11) < (long)(counts_1.length)) {
            if ((long)(counts_1[(int)((long)(i_11))]) > (long)(max_count_1)) {
                max_count_1 = (long)(counts_1[(int)((long)(i_11))]);
            }
            i_11 = (long)((long)(i_11) + 1L);
        }
        long[] modes_1 = ((long[])(new long[]{}));
        i_11 = 0L;
        while ((long)(i_11) < (long)(lst.length)) {
            if ((long)(counts_1[(int)((long)(i_11))]) == (long)(max_count_1)) {
                long v_1 = (long)(lst[(int)((long)(i_11))]);
                if (!(Boolean)contains_int(((long[])(modes_1)), (long)(v_1))) {
                    modes_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(modes_1), java.util.stream.LongStream.of((long)(v_1))).toArray()));
                }
            }
            i_11 = (long)((long)(i_11) + 1L);
        }
        return sort_int(((long[])(modes_1)));
    }

    static String[] mode_string(String[] lst) {
        if ((long)(lst.length) == 0L) {
            return new String[]{};
        }
        long[] counts_3 = ((long[])(new long[]{}));
        long i_13 = 0L;
        while ((long)(i_13) < (long)(lst.length)) {
            counts_3 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(counts_3), java.util.stream.LongStream.of((long)(count_string(((String[])(lst)), lst[(int)((long)(i_13))])))).toArray()));
            i_13 = (long)((long)(i_13) + 1L);
        }
        long max_count_3 = 0L;
        i_13 = 0L;
        while ((long)(i_13) < (long)(counts_3.length)) {
            if ((long)(counts_3[(int)((long)(i_13))]) > (long)(max_count_3)) {
                max_count_3 = (long)(counts_3[(int)((long)(i_13))]);
            }
            i_13 = (long)((long)(i_13) + 1L);
        }
        String[] modes_3 = ((String[])(new String[]{}));
        i_13 = 0L;
        while ((long)(i_13) < (long)(lst.length)) {
            if ((long)(counts_3[(int)((long)(i_13))]) == (long)(max_count_3)) {
                String v_3 = lst[(int)((long)(i_13))];
                if (!(Boolean)contains_string(((String[])(modes_3)), v_3)) {
                    modes_3 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(modes_3), java.util.stream.Stream.of(v_3)).toArray(String[]::new)));
                }
            }
            i_13 = (long)((long)(i_13) + 1L);
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
