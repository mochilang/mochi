public class Main {

    static int[] make_list(int len, int value) {
        int[] arr = ((int[])(new int[]{}));
        int i = 0;
        while (i < len) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return arr;
    }

    static int trapped_rainwater(int[] heights) {
        if (heights.length == 0) {
            return 0;
        }
        int i_1 = 0;
        while (i_1 < heights.length) {
            if (heights[i_1] < 0) {
                throw new RuntimeException(String.valueOf("No height can be negative"));
            }
            i_1 = i_1 + 1;
        }
        int length = heights.length;
        int[] left_max = ((int[])(make_list(length, 0)));
left_max[0] = heights[0];
        i_1 = 1;
        while (i_1 < length) {
            if (heights[i_1] > left_max[i_1 - 1]) {
left_max[i_1] = heights[i_1];
            } else {
left_max[i_1] = left_max[i_1 - 1];
            }
            i_1 = i_1 + 1;
        }
        int[] right_max = ((int[])(make_list(length, 0)));
        int last = length - 1;
right_max[last] = heights[last];
        i_1 = last - 1;
        while (i_1 >= 0) {
            if (heights[i_1] > right_max[i_1 + 1]) {
right_max[i_1] = heights[i_1];
            } else {
right_max[i_1] = right_max[i_1 + 1];
            }
            i_1 = i_1 - 1;
        }
        int total = 0;
        i_1 = 0;
        while (i_1 < length) {
            int left = left_max[i_1];
            int right = right_max[i_1];
            int smaller = left < right ? left : right;
            total = total + (smaller - heights[i_1]);
            i_1 = i_1 + 1;
        }
        return total;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(trapped_rainwater(((int[])(new int[]{0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1})))));
            System.out.println(_p(trapped_rainwater(((int[])(new int[]{7, 1, 5, 3, 6, 4})))));
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
