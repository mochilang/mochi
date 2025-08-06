public class Main {
    static int[][] GLIDER;
    static int[][] BLINKER;

    static int[][] new_generation(int[][] cells) {
        int rows = cells.length;
        int cols = cells[0].length;
        int[][] next = ((int[][])(new int[][]{}));
        int i = 0;
        while (i < rows) {
            int[] row = ((int[])(new int[]{}));
            int j = 0;
            while (j < cols) {
                int count = 0;
                if (i > 0 && j > 0) {
                    count = count + cells[i - 1][j - 1];
                }
                if (i > 0) {
                    count = count + cells[i - 1][j];
                }
                if (i > 0 && j < cols - 1) {
                    count = count + cells[i - 1][j + 1];
                }
                if (j > 0) {
                    count = count + cells[i][j - 1];
                }
                if (j < cols - 1) {
                    count = count + cells[i][j + 1];
                }
                if (i < rows - 1 && j > 0) {
                    count = count + cells[i + 1][j - 1];
                }
                if (i < rows - 1) {
                    count = count + cells[i + 1][j];
                }
                if (i < rows - 1 && j < cols - 1) {
                    count = count + cells[i + 1][j + 1];
                }
                boolean alive = cells[i][j] == 1;
                if ((alive && count >= 2 && count <= 3) || (!alive && count == 3)) {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(1)).toArray()));
                } else {
                    row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(0)).toArray()));
                }
                j = j + 1;
            }
            next = ((int[][])(appendObj(next, row)));
            i = i + 1;
        }
        return next;
    }

    static int[][][] generate_generations(int[][] cells, int frames) {
        int[][][] result = ((int[][][])(new int[][][]{}));
        int i_1 = 0;
        int[][] current = ((int[][])(cells));
        while (i_1 < frames) {
            result = ((int[][][])(appendObj(result, current)));
            current = ((int[][])(new_generation(((int[][])(current)))));
            i_1 = i_1 + 1;
        }
        return result;
    }

    static void main() {
        int[][][] frames = ((int[][][])(generate_generations(((int[][])(GLIDER)), 4)));
        int i_2 = 0;
        while (i_2 < frames.length) {
            System.out.println(java.util.Arrays.deepToString(frames[i_2]));
            i_2 = i_2 + 1;
        }
    }
    public static void main(String[] args) {
        GLIDER = ((int[][])(new int[][]{new int[]{0, 1, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 1, 0, 0, 0, 0, 0}, new int[]{1, 1, 1, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 0}, new int[]{0, 0, 0, 0, 0, 0, 0, 0}}));
        BLINKER = ((int[][])(new int[][]{new int[]{0, 1, 0}, new int[]{0, 1, 0}, new int[]{0, 1, 0}}));
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
