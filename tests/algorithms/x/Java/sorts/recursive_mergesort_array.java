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

    static int[] merge(int[] arr) {
        if (arr.length > 1) {
            int middle_length = Math.floorDiv(arr.length, 2);
            int[] left_array = ((int[])(subarray(((int[])(arr)), 0, middle_length)));
            int[] right_array = ((int[])(subarray(((int[])(arr)), middle_length, arr.length)));
            int left_size = left_array.length;
            int right_size = right_array.length;
            merge(((int[])(left_array)));
            merge(((int[])(right_array)));
            int left_index = 0;
            int right_index = 0;
            int index = 0;
            while (left_index < left_size && right_index < right_size) {
                if (left_array[left_index] < right_array[right_index]) {
arr[index] = left_array[left_index];
                    left_index = left_index + 1;
                } else {
arr[index] = right_array[right_index];
                    right_index = right_index + 1;
                }
                index = index + 1;
            }
            while (left_index < left_size) {
arr[index] = left_array[left_index];
                left_index = left_index + 1;
                index = index + 1;
            }
            while (right_index < right_size) {
arr[index] = right_array[right_index];
                right_index = right_index + 1;
                index = index + 1;
            }
        }
        return arr;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(merge(((int[])(new int[]{10, 9, 8, 7, 6, 5, 4, 3, 2, 1})))));
            System.out.println(_p(merge(((int[])(new int[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10})))));
            System.out.println(_p(merge(((int[])(new int[]{10, 22, 1, 2, 3, 9, 15, 23})))));
            System.out.println(_p(merge(((int[])(new int[]{100})))));
            System.out.println(_p(merge(((int[])(new int[]{})))));
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
