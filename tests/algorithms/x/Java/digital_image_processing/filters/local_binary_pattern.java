public class Main {
    static int[][] image;
    static int i_1 = 0;

    static int get_neighbors_pixel(int[][] image, int x, int y, int center) {
        if (x < 0 || y < 0) {
            return 0;
        }
        if (x >= image.length || y >= image[0].length) {
            return 0;
        }
        if (image[x][y] >= center) {
            return 1;
        }
        return 0;
    }

    static int local_binary_value(int[][] image, int x, int y) {
        int center = image[x][y];
        int[] powers = ((int[])(new int[]{1, 2, 4, 8, 16, 32, 64, 128}));
        int[] neighbors = ((int[])(new int[]{get_neighbors_pixel(((int[][])(image)), x - 1, y + 1, center), get_neighbors_pixel(((int[][])(image)), x, y + 1, center), get_neighbors_pixel(((int[][])(image)), x - 1, y, center), get_neighbors_pixel(((int[][])(image)), x + 1, y + 1, center), get_neighbors_pixel(((int[][])(image)), x + 1, y, center), get_neighbors_pixel(((int[][])(image)), x + 1, y - 1, center), get_neighbors_pixel(((int[][])(image)), x, y - 1, center), get_neighbors_pixel(((int[][])(image)), x - 1, y - 1, center)}));
        int sum = 0;
        int i = 0;
        while (i < neighbors.length) {
            sum = sum + neighbors[i] * powers[i];
            i = i + 1;
        }
        return sum;
    }
    public static void main(String[] args) {
        image = ((int[][])(new int[][]{new int[]{10, 10, 10, 10, 10}, new int[]{10, 20, 30, 20, 10}, new int[]{10, 30, 40, 30, 10}, new int[]{10, 20, 30, 20, 10}, new int[]{10, 10, 10, 10, 10}}));
        i_1 = 0;
        while (i_1 < image.length) {
            int j = 0;
            String line = "";
            while (j < image[0].length) {
                int value = local_binary_value(((int[][])(image)), i_1, j);
                line = line + _p(value);
                if (j < image[0].length - 1) {
                    line = line + " ";
                }
                j = j + 1;
            }
            System.out.println(line);
            i_1 = i_1 + 1;
        }
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
}
