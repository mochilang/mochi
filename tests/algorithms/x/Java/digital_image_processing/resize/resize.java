public class Main {

    static int[][][] zeros3d(int h, int w, int c) {
        int[][][] arr = ((int[][][])(new int[][][]{}));
        int y = 0;
        while (y < h) {
            int[][] row = ((int[][])(new int[][]{}));
            int x = 0;
            while (x < w) {
                int[] pixel = ((int[])(new int[]{}));
                int k = 0;
                while (k < c) {
                    pixel = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(pixel), java.util.stream.IntStream.of(0)).toArray()));
                    k = k + 1;
                }
                row = ((int[][])(appendObj(row, pixel)));
                x = x + 1;
            }
            arr = ((int[][][])(appendObj(arr, row)));
            y = y + 1;
        }
        return arr;
    }

    static int[][][] resize_nn(int[][][] img, int dst_w, int dst_h) {
        int src_h = img.length;
        int src_w = img[0].length;
        int channels = img[0][0].length;
        double ratio_x = (((Number)(src_w)).doubleValue()) / (((Number)(dst_w)).doubleValue());
        double ratio_y = (((Number)(src_h)).doubleValue()) / (((Number)(dst_h)).doubleValue());
        int[][][] out = ((int[][][])(zeros3d(dst_h, dst_w, channels)));
        int i = 0;
        while (i < dst_h) {
            int j = 0;
            while (j < dst_w) {
                int src_x = ((Number)((ratio_x * (((Number)(j)).doubleValue())))).intValue();
                int src_y = ((Number)((ratio_y * (((Number)(i)).doubleValue())))).intValue();
out[i][j] = ((int[])(img[src_y][src_x]));
                j = j + 1;
            }
            i = i + 1;
        }
        return out;
    }

    static void main() {
        int[][][] img = ((int[][][])(new int[][][]{new int[][]{new int[]{0, 0, 0}, new int[]{255, 255, 255}}, new int[][]{new int[]{255, 0, 0}, new int[]{0, 255, 0}}}));
        int[][][] resized = ((int[][][])(resize_nn(((int[][][])(img)), 4, 4)));
        System.out.println(java.util.Arrays.deepToString(resized));
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
