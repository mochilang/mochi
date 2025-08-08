public class Main {

    static int[][] segment_image(int[][] image, int[] thresholds) {
        int[][] segmented = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < image.length) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < image[i].length) {
                int pixel = image[i][j];
                int label = 0;
                int k = 0;
                while (k < thresholds.length) {
                    if (pixel > thresholds[k]) {
                        label = k + 1;
                    }
                    k = k + 1;
                }
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(label)).toArray()));
                j = j + 1;
            }
            segmented = ((int[][])(appendObj((int[][])segmented, row)));
            i = i + 1;
        }
        return segmented;
    }

    static void main() {
        int[][] image = ((int[][])(new int[][]{new int[]{80, 120, 180}, new int[]{40, 90, 150}, new int[]{20, 60, 100}}));
        int[] thresholds = ((int[])(new int[]{50, 100, 150}));
        int[][] segmented_1 = ((int[][])(segment_image(((int[][])(image)), ((int[])(thresholds)))));
        System.out.println(java.util.Arrays.deepToString(segmented_1));
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
