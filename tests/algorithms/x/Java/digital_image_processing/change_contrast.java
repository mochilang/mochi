public class Main {
    static int[][] image;
    static int[][] contrasted_1;

    static int[][] change_contrast(int[][] img, int level) {
        double factor = (259.0 * ((((Number)(level)).doubleValue()) + 255.0)) / (255.0 * (259.0 - (((Number)(level)).doubleValue())));
        int[][] result = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < img.length) {
            int[] row = ((int[])(img[i]));
            int[] new_row = ((int[])(new int[]{}));
            int j = 0;
            while (j < row.length) {
                int c = row[j];
                int contrasted = ((Number)((128.0 + factor * ((((Number)(c)).doubleValue()) - 128.0)))).intValue();
                int clamped = contrasted < 0 ? 0 : contrasted > 255 ? 255 : contrasted;
                new_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_row), java.util.stream.IntStream.of(clamped)).toArray()));
                j = j + 1;
            }
            result = ((int[][])(appendObj(result, new_row)));
            i = i + 1;
        }
        return result;
    }

    static void print_image(int[][] img) {
        int i_1 = 0;
        while (i_1 < img.length) {
            int[] row_1 = ((int[])(img[i_1]));
            int j_1 = 0;
            String line = "";
            while (j_1 < row_1.length) {
                line = line + _p(_geti(row_1, j_1)) + " ";
                j_1 = j_1 + 1;
            }
            System.out.println(line);
            i_1 = i_1 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            image = ((int[][])(new int[][]{new int[]{100, 125, 150}, new int[]{175, 200, 225}, new int[]{50, 75, 100}}));
            System.out.println("Original image:");
            print_image(((int[][])(image)));
            contrasted_1 = ((int[][])(change_contrast(((int[][])(image)), 170)));
            System.out.println("After contrast:");
            print_image(((int[][])(contrasted_1)));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
