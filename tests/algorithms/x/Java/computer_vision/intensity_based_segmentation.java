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
            segmented = ((int[][])(appendObj(segmented, row)));
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
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
