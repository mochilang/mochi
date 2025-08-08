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
                row = ((int[][])(appendObj((int[][])row, pixel)));
                x = x + 1;
            }
            arr = ((int[][][])(appendObj((int[][][])arr, row)));
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
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
