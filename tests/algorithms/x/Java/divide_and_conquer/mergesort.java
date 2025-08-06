public class Main {

    static int[] subarray(int[] xs, int start, int end) {
        int[] result = ((int[])(new int[]{}));
        int k = start;
        while (k < end) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(xs[k])).toArray()));
            k = k + 1;
        }
        return result;
    }

    static int[] merge(int[] left_half, int[] right_half) {
        int[] result_1 = ((int[])(new int[]{}));
        int i = 0;
        int j = 0;
        while (i < left_half.length && j < right_half.length) {
            if (left_half[i] < right_half[j]) {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(left_half[i])).toArray()));
                i = i + 1;
            } else {
                result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(right_half[j])).toArray()));
                j = j + 1;
            }
        }
        while (i < left_half.length) {
            result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(left_half[i])).toArray()));
            i = i + 1;
        }
        while (j < right_half.length) {
            result_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result_1), java.util.stream.IntStream.of(right_half[j])).toArray()));
            j = j + 1;
        }
        return result_1;
    }

    static int[] merge_sort(int[] array) {
        if (array.length <= 1) {
            return array;
        }
        int middle = array.length / 2;
        int[] left_half = ((int[])(subarray(((int[])(array)), 0, middle)));
        int[] right_half = ((int[])(subarray(((int[])(array)), middle, array.length)));
        int[] sorted_left = ((int[])(merge_sort(((int[])(left_half)))));
        int[] sorted_right = ((int[])(merge_sort(((int[])(right_half)))));
        return merge(((int[])(sorted_left)), ((int[])(sorted_right)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(merge_sort(((int[])(new int[]{5, 3, 1, 4, 2})))));
            System.out.println(_p(merge_sort(((int[])(new int[]{-2, 3, -10, 11, 99, 100000, 100, -200})))));
            System.out.println(_p(merge_sort(((int[])(new int[]{-200})))));
            System.out.println(_p(merge_sort(((int[])(new int[]{})))));
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

    static String _p(Object v) {
        if (v == null) return "<nil>";
        if (v.getClass().isArray()) {
            if (v instanceof int[]) return java.util.Arrays.toString((int[]) v);
            if (v instanceof long[]) return java.util.Arrays.toString((long[]) v);
            if (v instanceof double[]) return java.util.Arrays.toString((double[]) v);
            if (v instanceof boolean[]) return java.util.Arrays.toString((boolean[]) v);
            if (v instanceof byte[]) return java.util.Arrays.toString((byte[]) v);
            if (v instanceof char[]) return java.util.Arrays.toString((char[]) v);
            if (v instanceof short[]) return java.util.Arrays.toString((short[]) v);
            if (v instanceof float[]) return java.util.Arrays.toString((float[]) v);
            return java.util.Arrays.deepToString((Object[]) v);
        }
        return String.valueOf(v);
    }
}
