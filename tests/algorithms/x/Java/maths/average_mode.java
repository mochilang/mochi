public class Main {

    static boolean contains_int(int[] xs, int x) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == x) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static boolean contains_string(String[] xs, String x) {
        int i_1 = 0;
        while (i_1 < xs.length) {
            if ((xs[i_1].equals(x))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static int count_int(int[] xs, int x) {
        int cnt = 0;
        int i_2 = 0;
        while (i_2 < xs.length) {
            if (xs[i_2] == x) {
                cnt = cnt + 1;
            }
            i_2 = i_2 + 1;
        }
        return cnt;
    }

    static int count_string(String[] xs, String x) {
        int cnt_1 = 0;
        int i_3 = 0;
        while (i_3 < xs.length) {
            if ((xs[i_3].equals(x))) {
                cnt_1 = cnt_1 + 1;
            }
            i_3 = i_3 + 1;
        }
        return cnt_1;
    }

    static int[] sort_int(int[] xs) {
        int[] arr = ((int[])(xs));
        int i_4 = 0;
        while (i_4 < arr.length) {
            int j = i_4 + 1;
            while (j < arr.length) {
                if (arr[j] < arr[i_4]) {
                    int tmp = arr[i_4];
arr[i_4] = arr[j];
arr[j] = tmp;
                }
                j = j + 1;
            }
            i_4 = i_4 + 1;
        }
        return arr;
    }

    static String[] sort_string(String[] xs) {
        String[] arr_1 = ((String[])(xs));
        int i_5 = 0;
        while (i_5 < arr_1.length) {
            int j_1 = i_5 + 1;
            while (j_1 < arr_1.length) {
                if ((arr_1[j_1].compareTo(arr_1[i_5]) < 0)) {
                    String tmp_1 = arr_1[i_5];
arr_1[i_5] = arr_1[j_1];
arr_1[j_1] = tmp_1;
                }
                j_1 = j_1 + 1;
            }
            i_5 = i_5 + 1;
        }
        return arr_1;
    }

    static int[] mode_int(int[] lst) {
        if (lst.length == 0) {
            return new int[]{};
        }
        int[] counts = ((int[])(new int[]{}));
        int i_6 = 0;
        while (i_6 < lst.length) {
            counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(count_int(((int[])(lst)), lst[i_6]))).toArray()));
            i_6 = i_6 + 1;
        }
        int max_count = 0;
        i_6 = 0;
        while (i_6 < counts.length) {
            if (counts[i_6] > max_count) {
                max_count = counts[i_6];
            }
            i_6 = i_6 + 1;
        }
        int[] modes = ((int[])(new int[]{}));
        i_6 = 0;
        while (i_6 < lst.length) {
            if (counts[i_6] == max_count) {
                int v = lst[i_6];
                if (!(Boolean)contains_int(((int[])(modes)), v)) {
                    modes = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(modes), java.util.stream.IntStream.of(v)).toArray()));
                }
            }
            i_6 = i_6 + 1;
        }
        return sort_int(((int[])(modes)));
    }

    static String[] mode_string(String[] lst) {
        if (lst.length == 0) {
            return new String[]{};
        }
        int[] counts_1 = ((int[])(new int[]{}));
        int i_7 = 0;
        while (i_7 < lst.length) {
            counts_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts_1), java.util.stream.IntStream.of(count_string(((String[])(lst)), lst[i_7]))).toArray()));
            i_7 = i_7 + 1;
        }
        int max_count_1 = 0;
        i_7 = 0;
        while (i_7 < counts_1.length) {
            if (counts_1[i_7] > max_count_1) {
                max_count_1 = counts_1[i_7];
            }
            i_7 = i_7 + 1;
        }
        String[] modes_1 = ((String[])(new String[]{}));
        i_7 = 0;
        while (i_7 < lst.length) {
            if (counts_1[i_7] == max_count_1) {
                String v_1 = lst[i_7];
                if (!(Boolean)contains_string(((String[])(modes_1)), v_1)) {
                    modes_1 = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(modes_1), java.util.stream.Stream.of(v_1)).toArray(String[]::new)));
                }
            }
            i_7 = i_7 + 1;
        }
        return sort_string(((String[])(modes_1)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(mode_int(((int[])(new int[]{2, 3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 2, 2, 2}))));
            System.out.println(mode_int(((int[])(new int[]{3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 2, 2, 2}))));
            System.out.println(mode_int(((int[])(new int[]{3, 4, 5, 3, 4, 2, 5, 2, 2, 4, 4, 4, 2, 2, 4, 2}))));
            System.out.println(mode_string(((String[])(new String[]{"x", "y", "y", "z"}))));
            System.out.println(mode_string(((String[])(new String[]{"x", "x", "y", "y", "z"}))));
            long _benchDuration = _now() - _benchStart;
            long _benchMemory = _mem() - _benchMem;
            System.out.println("{");
            System.out.println("  \"duration_us\": " + _benchDuration + ",");
            System.out.println("  \"memory_bytes\": " + _benchMemory + ",");
            System.out.println("  \"name\": \"main\"");
            System.out.println("}");
            return;
        }
    }

    static boolean _nowSeeded = false;
    static int _nowSeed;
    static int _now() {
        if (!_nowSeeded) {
            String s = System.getenv("MOCHI_NOW_SEED");
            if (s != null && !s.isEmpty()) {
                try { _nowSeed = Integer.parseInt(s); _nowSeeded = true; } catch (Exception e) {}
            }
        }
        if (_nowSeeded) {
            _nowSeed = (int)((_nowSeed * 1664525L + 1013904223) % 2147483647);
            return _nowSeed;
        }
        return (int)(System.nanoTime() / 1000);
    }

    static long _mem() {
        Runtime rt = Runtime.getRuntime();
        rt.gc();
        return rt.totalMemory() - rt.freeMemory();
    }
}
