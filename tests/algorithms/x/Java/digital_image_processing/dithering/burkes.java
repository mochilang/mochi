public class Main {

    static int get_greyscale(int blue, int green, int red) {
        double b = ((Number)(blue)).doubleValue();
        double g = ((Number)(green)).doubleValue();
        double r = ((Number)(red)).doubleValue();
        return ((Number)((0.114 * b + 0.587 * g + 0.299 * r))).intValue();
    }

    static int[][] zeros(int h, int w) {
        int[][] table = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < h) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < w) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                j = j + 1;
            }
            table = ((int[][])(appendObj(table, row)));
            i = i + 1;
        }
        return table;
    }

    static int[][] burkes_dither(int[][][] img, int threshold) {
        int height = img.length;
        int width = img[0].length;
        int[][] error_table = ((int[][])(zeros(height + 1, width + 4)));
        int[][] output = ((int[][])(new int[][]{}));
        int y = 0;
        while (y < height) {
            int[] row_1 = ((int[])(new int[]{}));
            int x = 0;
            while (x < width) {
                int[] px = ((int[])(img[y][x]));
                int grey = get_greyscale(px[0], px[1], px[2]);
                int total = grey + error_table[y][x + 2];
                int new_val = 0;
                int current_error = 0;
                if (threshold > total) {
                    new_val = 0;
                    current_error = total;
                } else {
                    new_val = 255;
                    current_error = total - 255;
                }
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(new_val)).toArray()));
error_table[y][x + 3] = error_table[y][x + 3] + (8 * current_error) / 32;
error_table[y][x + 4] = error_table[y][x + 4] + (4 * current_error) / 32;
error_table[y + 1][x + 2] = error_table[y + 1][x + 2] + (8 * current_error) / 32;
error_table[y + 1][x + 3] = error_table[y + 1][x + 3] + (4 * current_error) / 32;
error_table[y + 1][x + 4] = error_table[y + 1][x + 4] + (2 * current_error) / 32;
error_table[y + 1][x + 1] = error_table[y + 1][x + 1] + (4 * current_error) / 32;
error_table[y + 1][x] = error_table[y + 1][x] + (2 * current_error) / 32;
                x = x + 1;
            }
            output = ((int[][])(appendObj(output, row_1)));
            y = y + 1;
        }
        return output;
    }

    static void main() {
        int[][][] img = ((int[][][])(new int[][][]{new int[][]{new int[]{0, 0, 0}, new int[]{64, 64, 64}, new int[]{128, 128, 128}, new int[]{192, 192, 192}}, new int[][]{new int[]{255, 255, 255}, new int[]{200, 200, 200}, new int[]{150, 150, 150}, new int[]{100, 100, 100}}, new int[][]{new int[]{30, 144, 255}, new int[]{255, 0, 0}, new int[]{0, 255, 0}, new int[]{0, 0, 255}}, new int[][]{new int[]{50, 100, 150}, new int[]{80, 160, 240}, new int[]{70, 140, 210}, new int[]{60, 120, 180}}}));
        int[][] result = ((int[][])(burkes_dither(((int[][][])(img)), 128)));
        int y_1 = 0;
        while (y_1 < result.length) {
            String line = "";
            int x_1 = 0;
            while (x_1 < result[y_1].length) {
                line = line + _p(_geti(result[y_1], x_1));
                if (x_1 < result[y_1].length - 1) {
                    line = line + " ";
                }
                x_1 = x_1 + 1;
            }
            System.out.println(line);
            y_1 = y_1 + 1;
        }
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
