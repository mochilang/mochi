public class Main {
    static int[][] img;
    static int[][] result;

    static int[] make_list(int n, int value) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(value)).toArray()));
            i = i + 1;
        }
        return res;
    }

    static int[][] histogram_stretch(int[][] image) {
        int height = image.length;
        int width = image[0].length;
        int[] hist = ((int[])(make_list(256, 0)));
        int i_1 = 0;
        while (i_1 < height) {
            int j = 0;
            while (j < width) {
                int val = image[i_1][j];
hist[val] = hist[val] + 1;
                j = j + 1;
            }
            i_1 = i_1 + 1;
        }
        int[] mapping = ((int[])(make_list(256, 0)));
        int cumulative = 0;
        int total = height * width;
        int h = 0;
        while (h < 256) {
            cumulative = cumulative + hist[h];
mapping[h] = Math.floorDiv((255 * cumulative), total);
            h = h + 1;
        }
        i_1 = 0;
        while (i_1 < height) {
            int j_1 = 0;
            while (j_1 < width) {
                int val_1 = image[i_1][j_1];
image[i_1][j_1] = mapping[val_1];
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return image;
    }

    static void print_image(int[][] image) {
        int i_2 = 0;
        while (i_2 < image.length) {
            System.out.println(java.util.Arrays.toString(image[i_2]));
            i_2 = i_2 + 1;
        }
    }
    public static void main(String[] args) {
        img = ((int[][])(new int[][]{new int[]{52, 55, 61}, new int[]{59, 79, 61}, new int[]{85, 76, 62}}));
        result = ((int[][])(histogram_stretch(((int[][])(img)))));
        print_image(((int[][])(result)));
    }
}
