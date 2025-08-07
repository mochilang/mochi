public class Main {
    static int[][] grid;

    static int max_product_four(int[][] grid) {
        int maximum = 0;
        int i = 0;
        while (i < 20) {
            int j = 0;
            while (j < 17) {
                int temp = grid[i][j] * grid[i][j + 1] * grid[i][j + 2] * grid[i][j + 3];
                if (temp > maximum) {
                    maximum = temp;
                }
                j = j + 1;
            }
            i = i + 1;
        }
        i = 0;
        while (i < 17) {
            int j_1 = 0;
            while (j_1 < 20) {
                int temp_1 = grid[i][j_1] * grid[i + 1][j_1] * grid[i + 2][j_1] * grid[i + 3][j_1];
                if (temp_1 > maximum) {
                    maximum = temp_1;
                }
                j_1 = j_1 + 1;
            }
            i = i + 1;
        }
        i = 0;
        while (i < 17) {
            int j_2 = 0;
            while (j_2 < 17) {
                int temp_2 = grid[i][j_2] * grid[i + 1][j_2 + 1] * grid[i + 2][j_2 + 2] * grid[i + 3][j_2 + 3];
                if (temp_2 > maximum) {
                    maximum = temp_2;
                }
                j_2 = j_2 + 1;
            }
            i = i + 1;
        }
        i = 0;
        while (i < 17) {
            int j_3 = 3;
            while (j_3 < 20) {
                int temp_3 = grid[i][j_3] * grid[i + 1][j_3 - 1] * grid[i + 2][j_3 - 2] * grid[i + 3][j_3 - 3];
                if (temp_3 > maximum) {
                    maximum = temp_3;
                }
                j_3 = j_3 + 1;
            }
            i = i + 1;
        }
        return maximum;
    }
    public static void main(String[] args) {
        grid = ((int[][])(new int[][]{new int[]{8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8}, new int[]{49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0}, new int[]{81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65}, new int[]{52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91}, new int[]{22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80}, new int[]{24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50}, new int[]{32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70}, new int[]{67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21}, new int[]{24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72}, new int[]{21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95}, new int[]{78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92}, new int[]{16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57}, new int[]{86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58}, new int[]{19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40}, new int[]{4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66}, new int[]{88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69}, new int[]{4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36}, new int[]{20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16}, new int[]{20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54}, new int[]{1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48}}));
        System.out.println(_p(max_product_four(((int[][])(grid)))));
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
