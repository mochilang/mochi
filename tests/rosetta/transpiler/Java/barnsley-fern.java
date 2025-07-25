public class Main {
    static double xMin = -2.182;
    static double xMax = 2.6558;
    static double yMin = 0.0;
    static double yMax = 9.9983;
    static int width = 60;
    static int nIter = 10000;
    static double dx = xMax - xMin;
    static double dy = yMax - yMin;
    static int height = ((Number)((width * dy / dx))).intValue();
    static String[][] grid = new String[][]{};
    static int row = 0;
    static int seed = 1;
    static double x = 0.0;
    static double y = 0.0;
    static int ix = ((Number)(((((Number)(width)).doubleValue()) * (x - xMin) / dx))).intValue();
    static int iy = ((Number)(((((Number)(height)).doubleValue()) * (yMax - y) / dy))).intValue();
    static int i = 0;

    static int[] randInt(int s, int n) {
        int next = Math.floorMod((s * 1664525 + 1013904223), 2147483647);
        return new int[]{next, Math.floorMod(next, n)};
    }
    public static void main(String[] args) {
        while (row < height) {
            String[] line = new String[]{};
            int col = 0;
            while (col < width) {
                line = java.util.stream.Stream.concat(java.util.Arrays.stream(line), java.util.stream.Stream.of(" ")).toArray(String[]::new);
                col = col + 1;
            }
            grid = appendObj(grid, line);
            row = row + 1;
        }
        if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
grid[iy][ix] = "*";
        }
        while (i < nIter) {
            int[] res = randInt(seed, 100);
            seed = res[0];
            int r = res[1];
            if (r < 85) {
                double nx = 0.85 * x + 0.04 * y;
                double ny = -0.04 * x + 0.85 * y + 1.6;
                x = nx;
                y = ny;
            } else             if (r < 92) {
                double nx = 0.2 * x - 0.26 * y;
                double ny = 0.23 * x + 0.22 * y + 1.6;
                x = nx;
                y = ny;
            } else             if (r < 99) {
                double nx = -0.15 * x + 0.28 * y;
                double ny = 0.26 * x + 0.24 * y + 0.44;
                x = nx;
                y = ny;
            } else {
                x = 0.0;
                y = 0.16 * y;
            }
            ix = ((Number)(((((Number)(width)).doubleValue()) * (x - xMin) / dx))).intValue();
            iy = ((Number)(((((Number)(height)).doubleValue()) * (yMax - y) / dy))).intValue();
            if (ix >= 0 && ix < width && iy >= 0 && iy < height) {
grid[iy][ix] = "*";
            }
            i = i + 1;
        }
        row = 0;
        while (row < height) {
            String line = "";
            int col = 0;
            while (col < width) {
                line = String.valueOf(line + grid[row][col]);
                col = col + 1;
            }
            System.out.println(line);
            row = row + 1;
        }
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
