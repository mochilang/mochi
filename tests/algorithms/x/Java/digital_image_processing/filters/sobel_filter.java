public class Main {
    static double PI;

    static double absf(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double atanApprox(double x) {
        if (x > 1.0) {
            return PI / 2.0 - x / (x * x + 0.28);
        }
        if (x < (-1.0)) {
            return -PI / 2.0 - x / (x * x + 0.28);
        }
        return x / (1.0 + 0.28 * x * x);
    }

    static double atan2Approx(double y, double x) {
        if (x == 0.0) {
            if (y > 0.0) {
                return PI / 2.0;
            }
            if (y < 0.0) {
                return -PI / 2.0;
            }
            return 0.0;
        }
        double a = atanApprox(y / x);
        if (x > 0.0) {
            return a;
        }
        if (y >= 0.0) {
            return a + PI;
        }
        return a - PI;
    }

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

    static double[][] pad_edge(double[][] img, int pad) {
        int h = img.length;
        int w = img[0].length;
        double[][] out = ((double[][])(zeros(h + pad * 2, w + pad * 2)));
        int y_1 = 0;
        while (y_1 < h + pad * 2) {
            int x_1 = 0;
            while (x_1 < w + pad * 2) {
                int sy = y_1 - pad;
                if (sy < 0) {
                    sy = 0;
                }
                if (sy >= h) {
                    sy = h - 1;
                }
                int sx = x_1 - pad;
                if (sx < 0) {
                    sx = 0;
                }
                if (sx >= w) {
                    sx = w - 1;
                }
out[y_1][x_1] = img[sy][sx];
                x_1 = x_1 + 1;
            }
            y_1 = y_1 + 1;
        }
        return out;
    }

    static double[][] img_convolve(double[][] img, int[][] kernel) {
        int h_1 = img.length;
        int w_1 = img[0].length;
        int k = kernel.length;
        int pad = Math.floorDiv(k, 2);
        double[][] padded = ((double[][])(pad_edge(((double[][])(img)), pad)));
        double[][] out_1 = ((double[][])(zeros(h_1, w_1)));
        int y_2 = 0;
        while (y_2 < h_1) {
            int x_2 = 0;
            while (x_2 < w_1) {
                double sum = 0.0;
                int i_1 = 0;
                while (i_1 < k) {
                    int j = 0;
                    while (j < k) {
                        sum = sum + padded[y_2 + i_1][x_2 + j] * (((double)(kernel[i_1][j])));
                        j = j + 1;
                    }
                    i_1 = i_1 + 1;
                }
out_1[y_2][x_2] = sum;
                x_2 = x_2 + 1;
            }
            y_2 = y_2 + 1;
        }
        return out_1;
    }

    static double[][] abs_matrix(double[][] mat) {
        int h_2 = mat.length;
        int w_2 = mat[0].length;
        double[][] out_2 = ((double[][])(zeros(h_2, w_2)));
        int y_3 = 0;
        while (y_3 < h_2) {
            int x_3 = 0;
            while (x_3 < w_2) {
                double v = mat[y_3][x_3];
                if (v < 0.0) {
out_2[y_3][x_3] = -v;
                } else {
out_2[y_3][x_3] = v;
                }
                x_3 = x_3 + 1;
            }
            y_3 = y_3 + 1;
        }
        return out_2;
    }

    static double max_matrix(double[][] mat) {
        double max_val = mat[0][0];
        int y_4 = 0;
        while (y_4 < mat.length) {
            int x_4 = 0;
            while (x_4 < mat[0].length) {
                if (mat[y_4][x_4] > max_val) {
                    max_val = mat[y_4][x_4];
                }
                x_4 = x_4 + 1;
            }
            y_4 = y_4 + 1;
        }
        return max_val;
    }

    static double[][] scale_matrix(double[][] mat, double factor) {
        int h_3 = mat.length;
        int w_3 = mat[0].length;
        double[][] out_3 = ((double[][])(zeros(h_3, w_3)));
        int y_5 = 0;
        while (y_5 < h_3) {
            int x_5 = 0;
            while (x_5 < w_3) {
out_3[y_5][x_5] = mat[y_5][x_5] * factor;
                x_5 = x_5 + 1;
            }
            y_5 = y_5 + 1;
        }
        return out_3;
    }

    static double[][][] sobel_filter(int[][] image) {
        int h_4 = image.length;
        int w_4 = image[0].length;
        double[][] img = ((double[][])(new double[][]{}));
        int y0 = 0;
        while (y0 < h_4) {
            double[] row_1 = ((double[])(new double[]{}));
            int x0 = 0;
            while (x0 < w_4) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(((double)(image[y0][x0])))).toArray()));
                x0 = x0 + 1;
            }
            img = ((double[][])(appendObj((double[][])img, row_1)));
            y0 = y0 + 1;
        }
        int[][] kernel_x = ((int[][])(new int[][]{new int[]{-1, 0, 1}, new int[]{-2, 0, 2}, new int[]{-1, 0, 1}}));
        int[][] kernel_y = ((int[][])(new int[][]{new int[]{1, 2, 1}, new int[]{0, 0, 0}, new int[]{-1, -2, -1}}));
        double[][] dst_x = ((double[][])(abs_matrix(((double[][])(img_convolve(((double[][])(img)), ((int[][])(kernel_x))))))));
        double[][] dst_y = ((double[][])(abs_matrix(((double[][])(img_convolve(((double[][])(img)), ((int[][])(kernel_y))))))));
        double max_x = max_matrix(((double[][])(dst_x)));
        double max_y = max_matrix(((double[][])(dst_y)));
        dst_x = ((double[][])(scale_matrix(((double[][])(dst_x)), 255.0 / max_x)));
        dst_y = ((double[][])(scale_matrix(((double[][])(dst_y)), 255.0 / max_y)));
        double[][] mag = ((double[][])(zeros(h_4, w_4)));
        double[][] theta = ((double[][])(zeros(h_4, w_4)));
        int y_6 = 0;
        while (y_6 < h_4) {
            int x_6 = 0;
            while (x_6 < w_4) {
                double gx = dst_x[y_6][x_6];
                double gy = dst_y[y_6][x_6];
mag[y_6][x_6] = sqrtApprox(gx * gx + gy * gy);
theta[y_6][x_6] = atan2Approx(gy, gx);
                x_6 = x_6 + 1;
            }
            y_6 = y_6 + 1;
        }
        double max_m = max_matrix(((double[][])(mag)));
        mag = ((double[][])(scale_matrix(((double[][])(mag)), 255.0 / max_m)));
        return new double[][][]{mag, theta};
    }

    static void print_matrix_int(double[][] mat) {
        int y_7 = 0;
        while (y_7 < mat.length) {
            String line = "";
            int x_7 = 0;
            while (x_7 < mat[y_7].length) {
                line = line + _p(_getd(mat[y_7], x_7));
                if (x_7 < mat[y_7].length - 1) {
                    line = line + " ";
                }
                x_7 = x_7 + 1;
            }
            System.out.println(line);
            y_7 = y_7 + 1;
        }
    }

    static void print_matrix_float(double[][] mat) {
        int y_8 = 0;
        while (y_8 < mat.length) {
            String line_1 = "";
            int x_8 = 0;
            while (x_8 < mat[y_8].length) {
                line_1 = line_1 + _p(_getd(mat[y_8], x_8));
                if (x_8 < mat[y_8].length - 1) {
                    line_1 = line_1 + " ";
                }
                x_8 = x_8 + 1;
            }
            System.out.println(line_1);
            y_8 = y_8 + 1;
        }
    }

    static void main() {
        int[][] img_1 = ((int[][])(new int[][]{new int[]{10, 10, 10, 10, 10}, new int[]{10, 50, 50, 50, 10}, new int[]{10, 50, 80, 50, 10}, new int[]{10, 50, 50, 50, 10}, new int[]{10, 10, 10, 10, 10}}));
        double[][][] res = ((double[][][])(sobel_filter(((int[][])(img_1)))));
        double[][] mag_1 = ((double[][])(res[0]));
        double[][] theta_1 = ((double[][])(res[1]));
        print_matrix_int(((double[][])(mag_1)));
        print_matrix_float(((double[][])(theta_1)));
    }
    public static void main(String[] args) {
        PI = 3.141592653589793;
        main();
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
