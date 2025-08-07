public class Main {

    static int[] bubble_sort(int[] nums) {
        int[] arr = ((int[])(nums));
        int n = arr.length;
        int i = 0;
        while (i < n) {
            int j = 0;
            while (j < n - 1) {
                if (arr[j] > arr[j + 1]) {
                    int temp = arr[j];
arr[j] = arr[j + 1];
arr[j + 1] = temp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        return arr;
    }

    static int[][] three_sum(int[] nums) {
        int[] sorted = ((int[])(bubble_sort(((int[])(nums)))));
        int[][] res = ((int[][])(new int[][]{}));
        int n_1 = sorted.length;
        int i_1 = 0;
        while (i_1 < n_1 - 2) {
            if (i_1 == 0 || sorted[i_1] != sorted[i_1 - 1]) {
                int low = i_1 + 1;
                int high = n_1 - 1;
                int c = 0 - sorted[i_1];
                while (low < high) {
                    int s = sorted[low] + sorted[high];
                    if (s == c) {
                        int[] triple = ((int[])(new int[]{sorted[i_1], sorted[low], sorted[high]}));
                        res = ((int[][])(appendObj(res, triple)));
                        while (low < high && sorted[low] == sorted[low + 1]) {
                            low = low + 1;
                        }
                        while (low < high && sorted[high] == sorted[high - 1]) {
                            high = high - 1;
                        }
                        low = low + 1;
                        high = high - 1;
                    } else                     if (s < c) {
                        low = low + 1;
                    } else {
                        high = high - 1;
                    }
                }
            }
            i_1 = i_1 + 1;
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(_p(three_sum(((int[])(new int[]{-1, 0, 1, 2, -1, -4})))));
            System.out.println(_p(three_sum(((int[])(new int[]{1, 2, 3, 4})))));
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
