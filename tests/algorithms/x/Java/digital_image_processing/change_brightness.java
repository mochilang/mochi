public class Main {
    static int[][] sample;

    static int clamp(int value) {
        if (value < 0) {
            return 0;
        }
        if (value > 255) {
            return 255;
        }
        return value;
    }

    static int[][] change_brightness(int[][] img, int level) {
        if (level < (-255) || level > 255) {
            throw new RuntimeException(String.valueOf("level must be between -255 and 255"));
        }
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < img.length) {
            int[] row_res = ((int[])(new int[]{}));
            int j = 0;
            while (j < img[i].length) {
                row_res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_res), java.util.stream.IntStream.of(clamp(img[i][j] + level))).toArray()));
                j = j + 1;
            }
            result = ((int[][])(appendObj(result, row_res)));
            i = i + 1;
        }
        return result;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            sample = ((int[][])(new int[][]{new int[]{100, 150}, new int[]{200, 250}}));
            System.out.println(change_brightness(((int[][])(sample)), 30));
            System.out.println(change_brightness(((int[][])(sample)), -60));
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
