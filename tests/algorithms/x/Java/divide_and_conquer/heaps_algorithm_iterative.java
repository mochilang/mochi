public class Main {

    static int[] copy_list(int[] arr) {
        int[] result = ((int[])(new int[]{}));
        int i = 0;
        while (i < arr.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(arr[i])).toArray()));
            i = i + 1;
        }
        return result;
    }

    static int[][] heaps(int[] arr) {
        if (arr.length <= 1) {
            int[][] single = ((int[][])(new int[][]{}));
            return appendObj(single, copy_list(((int[])(arr))));
        }
        int n = arr.length;
        int[] c = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < n) {
            c = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(c), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        int[][] res = ((int[][])(new int[][]{}));
        res = ((int[][])(appendObj(res, copy_list(((int[])(arr))))));
        i_1 = 0;
        while (i_1 < n) {
            if (c[i_1] < i_1) {
                if (Math.floorMod(i_1, 2) == 0) {
                    int temp = arr[0];
arr[0] = arr[i_1];
arr[i_1] = temp;
                } else {
                    int temp_1 = arr[c[i_1]];
arr[c[i_1]] = arr[i_1];
arr[i_1] = temp_1;
                }
                res = ((int[][])(appendObj(res, copy_list(((int[])(arr))))));
c[i_1] = c[i_1] + 1;
                i_1 = 0;
            } else {
c[i_1] = 0;
                i_1 = i_1 + 1;
            }
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(heaps(((int[])(new int[]{1, 2, 3})))));
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
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
