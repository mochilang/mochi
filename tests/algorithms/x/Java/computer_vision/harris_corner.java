public class Main {
    static int[][] img;
    static int[][] corners_1;

    static double[][] zeros(int h, int w) {
        double[][] m = ((double[][])(new double[][]{}));
        int y = 0;
        while (y < h) {
            double[] row = ((double[])(new double[]{}));
            int x = 0;
            while (x < w) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                x = x + 1;
            }
            m = ((double[][])(appendObj((double[][])m, row)));
            y = y + 1;
        }
        return m;
    }

    static double[][][] gradient(int[][] img) {
        int h = img.length;
        int w = img[0].length;
        double[][] dx = ((double[][])(zeros(h, w)));
        double[][] dy = ((double[][])(zeros(h, w)));
        int y_1 = 1;
        while (y_1 < h - 1) {
            int x_1 = 1;
            while (x_1 < w - 1) {
dx[y_1][x_1] = (((double)(img[y_1][x_1 + 1]))) - (((double)(img[y_1][x_1 - 1])));
dy[y_1][x_1] = (((double)(img[y_1 + 1][x_1]))) - (((double)(img[y_1 - 1][x_1])));
                x_1 = x_1 + 1;
            }
            y_1 = y_1 + 1;
        }
        return ((double[][][])(new double[][][]{dx, dy}));
    }

    static int[][] harris(int[][] img, double k, int window, double thresh) {
        int h_1 = img.length;
        int w_1 = img[0].length;
        double[][][] grads = ((double[][][])(gradient(((int[][])(img)))));
        double[][] dx_1 = ((double[][])(grads[0]));
        double[][] dy_1 = ((double[][])(grads[1]));
        double[][] ixx = ((double[][])(zeros(h_1, w_1)));
        double[][] iyy = ((double[][])(zeros(h_1, w_1)));
        double[][] ixy = ((double[][])(zeros(h_1, w_1)));
        int y_2 = 0;
        while (y_2 < h_1) {
            int x_2 = 0;
            while (x_2 < w_1) {
                double gx = dx_1[y_2][x_2];
                double gy = dy_1[y_2][x_2];
ixx[y_2][x_2] = gx * gx;
iyy[y_2][x_2] = gy * gy;
ixy[y_2][x_2] = gx * gy;
                x_2 = x_2 + 1;
            }
            y_2 = y_2 + 1;
        }
        int offset = Math.floorDiv(window, 2);
        int[][] corners = ((int[][])(new int[][]{}));
        y_2 = offset;
        while (y_2 < h_1 - offset) {
            int x_3 = offset;
            while (x_3 < w_1 - offset) {
                double wxx = 0.0;
                double wyy = 0.0;
                double wxy = 0.0;
                int yy = y_2 - offset;
                while (yy <= y_2 + offset) {
                    int xx = x_3 - offset;
                    while (xx <= x_3 + offset) {
                        wxx = wxx + ixx[yy][xx];
                        wyy = wyy + iyy[yy][xx];
                        wxy = wxy + ixy[yy][xx];
                        xx = xx + 1;
                    }
                    yy = yy + 1;
                }
                double det = wxx * wyy - (wxy * wxy);
                double trace = wxx + wyy;
                double r = det - k * (trace * trace);
                if (r > thresh) {
                    corners = ((int[][])(appendObj((int[][])corners, ((int[])(new int[]{x_3, y_2})))));
                }
                x_3 = x_3 + 1;
            }
            y_2 = y_2 + 1;
        }
        return corners;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            img = ((int[][])(new int[][]{new int[]{1, 1, 1, 1, 1}, new int[]{1, 255, 255, 255, 1}, new int[]{1, 255, 0, 255, 1}, new int[]{1, 255, 255, 255, 1}, new int[]{1, 1, 1, 1, 1}}));
            corners_1 = ((int[][])(harris(((int[][])(img)), 0.04, 3, 10000000000.0)));
            System.out.println(java.util.Arrays.deepToString(corners_1));
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
}
