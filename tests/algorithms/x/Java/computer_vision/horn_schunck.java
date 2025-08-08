public class Main {

    static int round_int(double x) {
        if (x >= 0.0) {
            return ((Number)(x + 0.5)).intValue();
        }
        return ((Number)(x - 0.5)).intValue();
    }

    static double[][] zeros(int rows, int cols) {
        double[][] res = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < rows) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < cols) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j = j + 1;
            }
            res = ((double[][])(appendObj((double[][])res, row)));
            i = i + 1;
        }
        return res;
    }

    static double[][] warp(double[][] image, double[][] h_flow, double[][] v_flow) {
        int h = image.length;
        int w = image[0].length;
        double[][] out = ((double[][])(new double[][]{}));
        int y = 0;
        while (y < h) {
            double[] row_1 = ((double[])(new double[]{}));
            int x = 0;
            while (x < w) {
                int sx = x - round_int(h_flow[y][x]);
                int sy = y - round_int(v_flow[y][x]);
                if (sx >= 0 && sx < w && sy >= 0 && sy < h) {
                    row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(image[sy][sx])).toArray()));
                } else {
                    row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
                x = x + 1;
            }
            out = ((double[][])(appendObj((double[][])out, row_1)));
            y = y + 1;
        }
        return out;
    }

    static double[][] convolve(double[][] img, double[][] ker) {
        int h_1 = img.length;
        int w_1 = img[0].length;
        int kh = ker.length;
        int kw = ker[0].length;
        int py = Math.floorDiv(kh, 2);
        int px = Math.floorDiv(kw, 2);
        double[][] out_1 = ((double[][])(new double[][]{}));
        int y_1 = 0;
        while (y_1 < h_1) {
            double[] row_2 = ((double[])(new double[]{}));
            int x_1 = 0;
            while (x_1 < w_1) {
                double s = 0.0;
                int ky = 0;
                while (ky < kh) {
                    int kx = 0;
                    while (kx < kw) {
                        int iy = y_1 + ky - py;
                        int ix = x_1 + kx - px;
                        if (iy >= 0 && iy < h_1 && ix >= 0 && ix < w_1) {
                            s = s + img[iy][ix] * ker[ky][kx];
                        }
                        kx = kx + 1;
                    }
                    ky = ky + 1;
                }
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(s)).toArray()));
                x_1 = x_1 + 1;
            }
            out_1 = ((double[][])(appendObj((double[][])out_1, row_2)));
            y_1 = y_1 + 1;
        }
        return out_1;
    }

    static double[][][] horn_schunck(double[][] image0, double[][] image1, int num_iter, double alpha) {
        int h_2 = image0.length;
        int w_2 = image0[0].length;
        double[][] u = ((double[][])(zeros(h_2, w_2)));
        double[][] v = ((double[][])(zeros(h_2, w_2)));
        double[][] kernel_x = ((double[][])(new double[][]{new double[]{-0.25, 0.25}, new double[]{-0.25, 0.25}}));
        double[][] kernel_y = ((double[][])(new double[][]{new double[]{-0.25, -0.25}, new double[]{0.25, 0.25}}));
        double[][] kernel_t = ((double[][])(new double[][]{new double[]{0.25, 0.25}, new double[]{0.25, 0.25}}));
        double[][] laplacian = ((double[][])(new double[][]{new double[]{0.0833333333333, 0.166666666667, 0.0833333333333}, new double[]{0.166666666667, 0.0, 0.166666666667}, new double[]{0.0833333333333, 0.166666666667, 0.0833333333333}}));
        int it = 0;
        while (it < num_iter) {
            double[][] warped = ((double[][])(warp(((double[][])(image0)), ((double[][])(u)), ((double[][])(v)))));
            double[][] dx1 = ((double[][])(convolve(((double[][])(warped)), ((double[][])(kernel_x)))));
            double[][] dx2 = ((double[][])(convolve(((double[][])(image1)), ((double[][])(kernel_x)))));
            double[][] dy1 = ((double[][])(convolve(((double[][])(warped)), ((double[][])(kernel_y)))));
            double[][] dy2 = ((double[][])(convolve(((double[][])(image1)), ((double[][])(kernel_y)))));
            double[][] dt1 = ((double[][])(convolve(((double[][])(warped)), ((double[][])(kernel_t)))));
            double[][] dt2 = ((double[][])(convolve(((double[][])(image1)), ((double[][])(kernel_t)))));
            double[][] avg_u = ((double[][])(convolve(((double[][])(u)), ((double[][])(laplacian)))));
            double[][] avg_v = ((double[][])(convolve(((double[][])(v)), ((double[][])(laplacian)))));
            int y_2 = 0;
            while (y_2 < h_2) {
                int x_2 = 0;
                while (x_2 < w_2) {
                    double dx = dx1[y_2][x_2] + dx2[y_2][x_2];
                    double dy = dy1[y_2][x_2] + dy2[y_2][x_2];
                    double dt = dt1[y_2][x_2] - dt2[y_2][x_2];
                    double au = avg_u[y_2][x_2];
                    double av = avg_v[y_2][x_2];
                    double numer = dx * au + dy * av + dt;
                    double denom = alpha * alpha + dx * dx + dy * dy;
                    double upd = numer / denom;
u[y_2][x_2] = au - dx * upd;
v[y_2][x_2] = av - dy * upd;
                    x_2 = x_2 + 1;
                }
                y_2 = y_2 + 1;
            }
            it = it + 1;
        }
        return new double[][][]{u, v};
    }

    static void print_matrix(double[][] mat) {
        int y_3 = 0;
        while (y_3 < mat.length) {
            double[] row_3 = ((double[])(mat[y_3]));
            int x_3 = 0;
            String line = "";
            while (x_3 < row_3.length) {
                line = line + _p(round_int(row_3[x_3]));
                if (x_3 + 1 < row_3.length) {
                    line = line + " ";
                }
                x_3 = x_3 + 1;
            }
            System.out.println(line);
            y_3 = y_3 + 1;
        }
    }

    static void main() {
        double[][] image0 = ((double[][])(new double[][]{new double[]{0.0, 0.0, 2.0}, new double[]{0.0, 0.0, 2.0}}));
        double[][] image1 = ((double[][])(new double[][]{new double[]{0.0, 2.0, 0.0}, new double[]{0.0, 2.0, 0.0}}));
        double[][][] flows = ((double[][][])(horn_schunck(((double[][])(image0)), ((double[][])(image1)), 20, 0.1)));
        double[][] u_1 = ((double[][])(flows[0]));
        double[][] v_1 = ((double[][])(flows[1]));
        print_matrix(((double[][])(u_1)));
        System.out.println("---");
        print_matrix(((double[][])(v_1)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
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
