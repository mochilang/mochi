public class Main {
    static int[][] img;
    static int[][] result;

    static int[][] mean_threshold(int[][] image) {
        int height = image.length;
        int width = image[0].length;
        int total = 0;
        int i = 0;
        while (i < height) {
            int j = 0;
            while (j < width) {
                total = total + image[i][j];
                j = j + 1;
            }
            i = i + 1;
        }
        int mean = total / (height * width);
        i = 0;
        while (i < height) {
            int j_1 = 0;
            while (j_1 < width) {
                if (image[i][j_1] > mean) {
image[i][j_1] = 255;
                } else {
image[i][j_1] = 0;
                }
                j_1 = j_1 + 1;
            }
            i = i + 1;
        }
        return image;
    }

    static void print_image(int[][] image) {
        int i_1 = 0;
        while (i_1 < image.length) {
            System.out.println(java.util.Arrays.toString(image[i_1]));
            i_1 = i_1 + 1;
        }
    }
    public static void main(String[] args) {
        img = ((int[][])(new int[][]{new int[]{10, 200, 50}, new int[]{100, 150, 30}, new int[]{90, 80, 220}}));
        result = ((int[][])(mean_threshold(((int[][])(img)))));
        print_image(((int[][])(result)));
    }
}
