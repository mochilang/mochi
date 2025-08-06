public class Main {

    static int[][][] convert_to_negative(int[][][] img) {
        int[][][] result = ((int[][][])(new int[][][]{}));
        int i = 0;
        while (i < img.length) {
            int[][] row = ((int[][])(new int[][]{}));
            int j = 0;
            while (j < img[i].length) {
                int[] pixel = ((int[])(img[i][j]));
                int r = 255 - pixel[0];
                int g = 255 - pixel[1];
                int b = 255 - pixel[2];
                row = ((int[][])(appendObj(row, new int[]{r, g, b})));
                j = j + 1;
            }
            result = ((int[][][])(appendObj(result, row)));
            i = i + 1;
        }
        return result;
    }

    static void main() {
        int[][][] image = ((int[][][])(new int[][][]{new int[][]{new int[]{10, 20, 30}, new int[]{0, 0, 0}}, new int[][]{new int[]{255, 255, 255}, new int[]{100, 150, 200}}}));
        int[][][] neg = ((int[][][])(convert_to_negative(((int[][][])(image)))));
        System.out.println(java.util.Arrays.deepToString(neg));
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
