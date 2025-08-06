public class Main {

    static int[] copy_list(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < xs.length) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i])).toArray()));
            i = i + 1;
        }
        return res;
    }

    static int[] longest_subsequence(int[] arr) {
        int n = arr.length;
        int[][] lis = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < n) {
            int[] single = ((int[])(new int[]{}));
            single = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(single), java.util.stream.IntStream.of(arr[i_1])).toArray()));
            lis = ((int[][])(appendObj(lis, single)));
            i_1 = i_1 + 1;
        }
        i_1 = 1;
        while (i_1 < n) {
            int prev = 0;
            while (prev < i_1) {
                if (arr[prev] <= arr[i_1] && lis[prev].length + 1 > lis[i_1].length) {
                    int[] temp = ((int[])(copy_list(((int[])(lis[prev])))));
                    int[] temp2 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(temp), java.util.stream.IntStream.of(arr[i_1])).toArray()));
lis[i_1] = ((int[])(temp2));
                }
                prev = prev + 1;
            }
            i_1 = i_1 + 1;
        }
        int[] result = ((int[])(new int[]{}));
        i_1 = 0;
        while (i_1 < n) {
            if (lis[i_1].length > result.length) {
                result = ((int[])(lis[i_1]));
            }
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void main() {
        System.out.println(_p(longest_subsequence(((int[])(new int[]{10, 22, 9, 33, 21, 50, 41, 60, 80})))));
        System.out.println(_p(longest_subsequence(((int[])(new int[]{4, 8, 7, 5, 1, 12, 2, 3, 9})))));
        System.out.println(_p(longest_subsequence(((int[])(new int[]{9, 8, 7, 6, 5, 7})))));
        System.out.println(_p(longest_subsequence(((int[])(new int[]{28, 26, 12, 23, 35, 39})))));
        System.out.println(_p(longest_subsequence(((int[])(new int[]{1, 1, 1})))));
        System.out.println(_p(longest_subsequence(((int[])(new int[]{})))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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
