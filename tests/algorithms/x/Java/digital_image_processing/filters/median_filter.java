public class Main {

    static int[] insertion_sort(int[] a) {
        int i = 1;
        while (i < a.length) {
            int key = a[i];
            int j = i - 1;
            while (j >= 0 && a[j] > key) {
a[j + 1] = a[j];
                j = j - 1;
            }
a[j + 1] = key;
            i = i + 1;
        }
        return a;
    }

    static int[][] median_filter(int[][] gray_img, int mask) {
        int rows = gray_img.length;
        int cols = gray_img[0].length;
        int bd = mask / 2;
        int[][] result = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < rows) {
            int[] row = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < cols) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j_1 = j_1 + 1;
            }
            result = ((int[][])(appendObj(result, row)));
            i_1 = i_1 + 1;
        }
        i_1 = bd;
        while (i_1 < rows - bd) {
            int j_2 = bd;
            while (j_2 < cols - bd) {
                int[] kernel = ((int[])(new int[]{}));
                int x = i_1 - bd;
                while (x <= i_1 + bd) {
                    int y = j_2 - bd;
                    while (y <= j_2 + bd) {
                        kernel = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(kernel), java.util.stream.IntStream.of(gray_img[x][y])).toArray()));
                        y = y + 1;
                    }
                    x = x + 1;
                }
                kernel = ((int[])(insertion_sort(((int[])(kernel)))));
                int idx = (mask * mask) / 2;
result[i_1][j_2] = kernel[idx];
                j_2 = j_2 + 1;
            }
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void main() {
        int[][] img = ((int[][])(new int[][]{new int[]{10, 10, 10, 10, 10}, new int[]{10, 255, 10, 255, 10}, new int[]{10, 10, 10, 10, 10}, new int[]{10, 255, 10, 255, 10}, new int[]{10, 10, 10, 10, 10}}));
        int[][] filtered = ((int[][])(median_filter(((int[][])(img)), 3)));
        System.out.println(java.util.Arrays.deepToString(filtered));
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
