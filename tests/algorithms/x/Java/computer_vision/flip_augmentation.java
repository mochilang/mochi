public class Main {
    static int[][] image;
    static double[][] boxes;
    static int[][] h_img;
    static double[][] h_boxes;
    static int[][] v_img;
    static double[][] v_boxes;

    static int[][] flip_horizontal_image(int[][] img) {
        int[][] flipped = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < img.length) {
            int[] row = ((int[])(img[i]));
            int j = row.length - 1;
            int[] new_row = ((int[])(new int[]{}));
            while (j >= 0) {
                new_row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_row), java.util.stream.IntStream.of(row[j])).toArray()));
                j = j - 1;
            }
            flipped = ((int[][])(appendObj((int[][])flipped, new_row)));
            i = i + 1;
        }
        return flipped;
    }

    static int[][] flip_vertical_image(int[][] img) {
        int[][] flipped_1 = ((int[][])(new int[][]{}));
        int i_1 = img.length - 1;
        while (i_1 >= 0) {
            flipped_1 = ((int[][])(appendObj((int[][])flipped_1, img[i_1])));
            i_1 = i_1 - 1;
        }
        return flipped_1;
    }

    static double[][] flip_horizontal_boxes(double[][] boxes) {
        Object result = new double[][]{};
        int i_2 = 0;
        while (i_2 < boxes.length) {
            double[] b = ((double[])(boxes[i_2]));
            double x_new = 1.0 - b[1];
            result = appendObj((double[][])result, new double[]{b[0], x_new, b[2], b[3], b[4]});
            i_2 = i_2 + 1;
        }
        return ((double[][])(result));
    }

    static double[][] flip_vertical_boxes(double[][] boxes) {
        Object result_1 = new double[][]{};
        int i_3 = 0;
        while (i_3 < boxes.length) {
            double[] b_1 = ((double[])(boxes[i_3]));
            double y_new = 1.0 - b_1[2];
            result_1 = appendObj((double[][])result_1, new double[]{b_1[0], b_1[1], y_new, b_1[3], b_1[4]});
            i_3 = i_3 + 1;
        }
        return ((double[][])(result_1));
    }

    static void print_image(int[][] img) {
        int i_4 = 0;
        while (i_4 < img.length) {
            int[] row_1 = ((int[])(img[i_4]));
            int j_1 = 0;
            String line = "";
            while (j_1 < row_1.length) {
                line = line + _p(_geti(row_1, j_1)) + " ";
                j_1 = j_1 + 1;
            }
            System.out.println(line);
            i_4 = i_4 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            image = ((int[][])(new int[][]{new int[]{1, 2, 3}, new int[]{4, 5, 6}, new int[]{7, 8, 9}}));
            boxes = ((double[][])(new double[][]{new double[]{0.0, 0.25, 0.25, 0.5, 0.5}, new double[]{1.0, 0.75, 0.75, 0.5, 0.5}}));
            System.out.println("Original image:");
            print_image(((int[][])(image)));
            System.out.println(_p(boxes));
            System.out.println("Horizontal flip:");
            h_img = ((int[][])(flip_horizontal_image(((int[][])(image)))));
            h_boxes = ((double[][])(flip_horizontal_boxes(((double[][])(boxes)))));
            print_image(((int[][])(h_img)));
            System.out.println(_p(h_boxes));
            System.out.println("Vertical flip:");
            v_img = ((int[][])(flip_vertical_image(((int[][])(image)))));
            v_boxes = ((double[][])(flip_vertical_boxes(((double[][])(boxes)))));
            print_image(((int[][])(v_img)));
            System.out.println(_p(v_boxes));
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
