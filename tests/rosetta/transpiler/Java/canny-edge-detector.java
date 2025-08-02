public class Main {
    static double PI;

    static double[][] conv2d(double[][] img, double[][] k) {
        int h = img.length;
        int w = img[0].length;
        int n = k.length;
        int half = n / 2;
        double[][] out = new double[][]{};
        int y = 0;
        while (y < h) {
            double[] row = new double[]{};
            int x = 0;
            while (x < w) {
                double sum = 0.0;
                int j = 0;
                while (j < n) {
                    int i = 0;
                    while (i < n) {
                        int yy = y + j - half;
                        if (yy < 0) {
                            yy = 0;
                        }
                        if (yy >= h) {
                            yy = h - 1;
                        }
                        int xx = x + i - half;
                        if (xx < 0) {
                            xx = 0;
                        }
                        if (xx >= w) {
                            xx = w - 1;
                        }
                        sum = sum + img[yy][xx] * k[j][i];
                        i = i + 1;
                    }
                    j = j + 1;
                }
                row = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(sum)).toArray();
                x = x + 1;
            }
            out = appendObj(out, row);
            y = y + 1;
        }
        return out;
    }

    static double[][] gradient(double[][] img) {
        double[][] hx = new double[][]{new double[]{-1.0, 0.0, 1.0}, new double[]{-2.0, 0.0, 2.0}, new double[]{-1.0, 0.0, 1.0}};
        double[][] hy = new double[][]{new double[]{1.0, 2.0, 1.0}, new double[]{0.0, 0.0, 0.0}, new double[]{-1.0, -2.0, -1.0}};
        double[][] gx = conv2d(img, hx);
        double[][] gy = conv2d(img, hy);
        int h_1 = img.length;
        int w_1 = img[0].length;
        double[][] out_1 = new double[][]{};
        int y_1 = 0;
        while (y_1 < h_1) {
            double[] row_1 = new double[]{};
            int x_1 = 0;
            while (x_1 < w_1) {
                double g = gx[y_1][x_1] * gx[y_1][x_1] + gy[y_1][x_1] * gy[y_1][x_1];
                row_1 = java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(g)).toArray();
                x_1 = x_1 + 1;
            }
            out_1 = appendObj(out_1, row_1);
            y_1 = y_1 + 1;
        }
        return out_1;
    }

    static int[][] threshold(double[][] g, double t) {
        int h_2 = g.length;
        int w_2 = g[0].length;
        int[][] out_2 = new int[][]{};
        int y_2 = 0;
        while (y_2 < h_2) {
            int[] row_2 = new int[]{};
            int x_2 = 0;
            while (x_2 < w_2) {
                if (g[y_2][x_2] >= t) {
                    row_2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(1)).toArray();
                } else {
                    row_2 = java.util.stream.IntStream.concat(java.util.Arrays.stream(row_2), java.util.stream.IntStream.of(0)).toArray();
                }
                x_2 = x_2 + 1;
            }
            out_2 = appendObj(out_2, row_2);
            y_2 = y_2 + 1;
        }
        return out_2;
    }

    static void printMatrix(int[][] m) {
        int y_3 = 0;
        while (y_3 < m.length) {
            String line = "";
            int x_3 = 0;
            while (x_3 < m[0].length) {
                line = line + _p(_geti(m[y_3], x_3));
                if (x_3 < m[0].length - 1) {
                    line = line + " ";
                }
                x_3 = x_3 + 1;
            }
            System.out.println(line);
            y_3 = y_3 + 1;
        }
    }

    static void main() {
        double[][] img = new double[][]{new double[]{0.0, 0.0, 0.0, 0.0, 0.0}, new double[]{0.0, 255.0, 255.0, 255.0, 0.0}, new double[]{0.0, 255.0, 255.0, 255.0, 0.0}, new double[]{0.0, 255.0, 255.0, 255.0, 0.0}, new double[]{0.0, 0.0, 0.0, 0.0, 0.0}};
        double[][] g_1 = gradient(img);
        int[][] edges = threshold(g_1, 1020.0 * 1020.0);
        printMatrix(edges);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            main();
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
        return v != null ? String.valueOf(v) : "<nil>";
    }

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
