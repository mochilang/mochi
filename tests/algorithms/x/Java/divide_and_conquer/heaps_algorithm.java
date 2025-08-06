public class Main {

    static int[][] permute(int k, int[] arr, int[][] res) {
        if (k == 1) {
            int[] copy = ((int[])(java.util.Arrays.copyOfRange(arr, 0, arr.length)));
            return appendObj(res, copy);
        }
        res = ((int[][])(permute(k - 1, ((int[])(arr)), ((int[][])(res)))));
        int i = 0;
        while (i < k - 1) {
            if (Math.floorMod(k, 2) == 0) {
                int temp = arr[i];
arr[i] = arr[k - 1];
arr[k - 1] = temp;
            } else {
                int temp_1 = arr[0];
arr[0] = arr[k - 1];
arr[k - 1] = temp_1;
            }
            res = ((int[][])(permute(k - 1, ((int[])(arr)), ((int[][])(res)))));
            i = i + 1;
        }
        return res;
    }

    static int[][] heaps(int[] arr) {
        if (arr.length <= 1) {
            return new int[][]{java.util.Arrays.copyOfRange(arr, 0, arr.length)};
        }
        int[][] res = ((int[][])(new int[][]{}));
        res = ((int[][])(permute(arr.length, ((int[])(arr)), ((int[][])(res)))));
        return res;
    }

    static void main() {
        int[][] perms = ((int[][])(heaps(((int[])(new int[]{1, 2, 3})))));
        System.out.println(java.util.Arrays.deepToString(perms));
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
}
