public class Main {
    static int[][] img;
    static int[][] negative;
    static int[][] contrast;
    static double[][] kernel;
    static double[][] laplace;
    static int[][] convolved;
    static int[][] medianed;
    static int[][] sobel;
    static int[][] lbp_img;

    static int clamp_byte(int x) {
        if (x < 0) {
            return 0;
        }
        if (x > 255) {
            return 255;
        }
        return x;
    }

    static int[][] convert_to_negative(int[][] img) {
        int h = img.length;
        int w = img[0].length;
        int[][] out = ((int[][])(new int[][]{}));
        int y = 0;
        while (y < h) {
            int[] row = ((int[])(new int[]{}));
            int x = 0;
            while (x < w) {
                row = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row), java.util.stream.IntStream.of(255 - img[y][x])).toArray()));
                x = x + 1;
            }
            out = ((int[][])(appendObj((int[][])out, row)));
            y = y + 1;
        }
        return out;
    }

    static int[][] change_contrast(int[][] img, int factor) {
        int h_1 = img.length;
        int w_1 = img[0].length;
        int[][] out_1 = ((int[][])(new int[][]{}));
        int y_1 = 0;
        while (y_1 < h_1) {
            int[] row_1 = ((int[])(new int[]{}));
            int x_1 = 0;
            while (x_1 < w_1) {
                int p = img[y_1][x_1];
                int v = Math.floorDiv(((p - 128) * factor), 100) + 128;
                v = clamp_byte(v);
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(v)).toArray()));
                x_1 = x_1 + 1;
            }
            out_1 = ((int[][])(appendObj((int[][])out_1, row_1)));
            y_1 = y_1 + 1;
        }
        return out_1;
    }

    static double[][] gen_gaussian_kernel(int n, double sigma) {
        if (n == 3) {
            return new double[][]{new double[]{1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0}, new double[]{2.0 / 16.0, 4.0 / 16.0, 2.0 / 16.0}, new double[]{1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0}};
        }
        double[][] k = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < n) {
            double[] row_2 = ((double[])(new double[]{}));
            int j = 0;
            while (j < n) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j = j + 1;
            }
            k = ((double[][])(appendObj((double[][])k, row_2)));
            i = i + 1;
        }
        return k;
    }

    static int[][] img_convolve(int[][] img, double[][] kernel) {
        int h_2 = img.length;
        int w_2 = img[0].length;
        int[][] out_2 = ((int[][])(new int[][]{}));
        int y_2 = 0;
        while (y_2 < h_2) {
            int[] row_3 = ((int[])(new int[]{}));
            int x_2 = 0;
            while (x_2 < w_2) {
                double acc = 0.0;
                int ky = 0;
                while (ky < kernel.length) {
                    int kx = 0;
                    while (kx < kernel[0].length) {
                        int iy = y_2 + ky - 1;
                        int ix = x_2 + kx - 1;
                        int pixel = 0;
                        if (iy >= 0 && iy < h_2 && ix >= 0 && ix < w_2) {
                            pixel = img[iy][ix];
                        }
                        acc = acc + kernel[ky][kx] * (1.0 * pixel);
                        kx = kx + 1;
                    }
                    ky = ky + 1;
                }
                row_3 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_3), java.util.stream.IntStream.of(((Number)(acc)).intValue())).toArray()));
                x_2 = x_2 + 1;
            }
            out_2 = ((int[][])(appendObj((int[][])out_2, row_3)));
            y_2 = y_2 + 1;
        }
        return out_2;
    }

    static int[] sort_ints(int[] xs) {
        int[] arr = ((int[])(xs));
        int i_1 = 0;
        while (i_1 < arr.length) {
            int j_1 = 0;
            while (j_1 < arr.length - 1 - i_1) {
                if (arr[j_1] > arr[j_1 + 1]) {
                    int tmp = arr[j_1];
arr[j_1] = arr[j_1 + 1];
arr[j_1 + 1] = tmp;
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return arr;
    }

    static int[][] median_filter(int[][] img, int k) {
        int h_3 = img.length;
        int w_3 = img[0].length;
        int offset = Math.floorDiv(k, 2);
        int[][] out_3 = ((int[][])(new int[][]{}));
        int y_3 = 0;
        while (y_3 < h_3) {
            int[] row_4 = ((int[])(new int[]{}));
            int x_3 = 0;
            while (x_3 < w_3) {
                int[] vals = ((int[])(new int[]{}));
                int ky_1 = 0;
                while (ky_1 < k) {
                    int kx_1 = 0;
                    while (kx_1 < k) {
                        int iy_1 = y_3 + ky_1 - offset;
                        int ix_1 = x_3 + kx_1 - offset;
                        int pixel_1 = 0;
                        if (iy_1 >= 0 && iy_1 < h_3 && ix_1 >= 0 && ix_1 < w_3) {
                            pixel_1 = img[iy_1][ix_1];
                        }
                        vals = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(vals), java.util.stream.IntStream.of(pixel_1)).toArray()));
                        kx_1 = kx_1 + 1;
                    }
                    ky_1 = ky_1 + 1;
                }
                int[] sorted = ((int[])(sort_ints(((int[])(vals)))));
                row_4 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_4), java.util.stream.IntStream.of(sorted[Math.floorDiv(sorted.length, 2)])).toArray()));
                x_3 = x_3 + 1;
            }
            out_3 = ((int[][])(appendObj((int[][])out_3, row_4)));
            y_3 = y_3 + 1;
        }
        return out_3;
    }

    static int iabs(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static int[][] sobel_filter(int[][] img) {
        int[][] gx = ((int[][])(new int[][]{new int[]{1, 0, -1}, new int[]{2, 0, -2}, new int[]{1, 0, -1}}));
        int[][] gy = ((int[][])(new int[][]{new int[]{1, 2, 1}, new int[]{0, 0, 0}, new int[]{-1, -2, -1}}));
        int h_4 = img.length;
        int w_4 = img[0].length;
        int[][] out_4 = ((int[][])(new int[][]{}));
        int y_4 = 0;
        while (y_4 < h_4) {
            int[] row_5 = ((int[])(new int[]{}));
            int x_4 = 0;
            while (x_4 < w_4) {
                int sx = 0;
                int sy = 0;
                int ky_2 = 0;
                while (ky_2 < 3) {
                    int kx_2 = 0;
                    while (kx_2 < 3) {
                        int iy_2 = y_4 + ky_2 - 1;
                        int ix_2 = x_4 + kx_2 - 1;
                        int pixel_2 = 0;
                        if (iy_2 >= 0 && iy_2 < h_4 && ix_2 >= 0 && ix_2 < w_4) {
                            pixel_2 = img[iy_2][ix_2];
                        }
                        sx = sx + gx[ky_2][kx_2] * pixel_2;
                        sy = sy + gy[ky_2][kx_2] * pixel_2;
                        kx_2 = kx_2 + 1;
                    }
                    ky_2 = ky_2 + 1;
                }
                row_5 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_5), java.util.stream.IntStream.of(iabs(sx) + iabs(sy))).toArray()));
                x_4 = x_4 + 1;
            }
            out_4 = ((int[][])(appendObj((int[][])out_4, row_5)));
            y_4 = y_4 + 1;
        }
        return out_4;
    }

    static int[] get_neighbors_pixel(int[][] img, int x, int y) {
        int h_5 = img.length;
        int w_5 = img[0].length;
        int[] neighbors = ((int[])(new int[]{}));
        int dy = -1;
        while (dy <= 1) {
            int dx = -1;
            while (dx <= 1) {
                if (!(dx == 0 && dy == 0)) {
                    int ny = y + dy;
                    int nx = x + dx;
                    int val = 0;
                    if (ny >= 0 && ny < h_5 && nx >= 0 && nx < w_5) {
                        val = img[ny][nx];
                    }
                    neighbors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(neighbors), java.util.stream.IntStream.of(val)).toArray()));
                }
                dx = dx + 1;
            }
            dy = dy + 1;
        }
        return neighbors;
    }

    static int pow2(int e) {
        int r = 1;
        int i_2 = 0;
        while (i_2 < e) {
            r = r * 2;
            i_2 = i_2 + 1;
        }
        return r;
    }

    static int local_binary_value(int[][] img, int x, int y) {
        int center = img[y][x];
        int[] neighbors_1 = ((int[])(get_neighbors_pixel(((int[][])(img)), x, y)));
        int v_1 = 0;
        int i_3 = 0;
        while (i_3 < neighbors_1.length) {
            if (neighbors_1[i_3] >= center) {
                v_1 = v_1 + pow2(i_3);
            }
            i_3 = i_3 + 1;
        }
        return v_1;
    }

    static int[][] local_binary_pattern(int[][] img) {
        int h_6 = img.length;
        int w_6 = img[0].length;
        int[][] out_5 = ((int[][])(new int[][]{}));
        int y_5 = 0;
        while (y_5 < h_6) {
            int[] row_6 = ((int[])(new int[]{}));
            int x_5 = 0;
            while (x_5 < w_6) {
                row_6 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_6), java.util.stream.IntStream.of(local_binary_value(((int[][])(img)), x_5, y_5))).toArray()));
                x_5 = x_5 + 1;
            }
            out_5 = ((int[][])(appendObj((int[][])out_5, row_6)));
            y_5 = y_5 + 1;
        }
        return out_5;
    }
    public static void main(String[] args) {
        img = ((int[][])(new int[][]{new int[]{52, 55, 61}, new int[]{62, 59, 55}, new int[]{63, 65, 66}}));
        negative = ((int[][])(convert_to_negative(((int[][])(img)))));
        contrast = ((int[][])(change_contrast(((int[][])(img)), 110)));
        kernel = ((double[][])(gen_gaussian_kernel(3, 1.0)));
        laplace = ((double[][])(new double[][]{new double[]{0.25, 0.5, 0.25}, new double[]{0.5, -3.0, 0.5}, new double[]{0.25, 0.5, 0.25}}));
        convolved = ((int[][])(img_convolve(((int[][])(img)), ((double[][])(laplace)))));
        medianed = ((int[][])(median_filter(((int[][])(img)), 3)));
        sobel = ((int[][])(sobel_filter(((int[][])(img)))));
        lbp_img = ((int[][])(local_binary_pattern(((int[][])(img)))));
        System.out.println(java.util.Arrays.deepToString(negative));
        System.out.println(java.util.Arrays.deepToString(contrast));
        System.out.println(java.util.Arrays.deepToString(kernel));
        System.out.println(java.util.Arrays.deepToString(convolved));
        System.out.println(java.util.Arrays.deepToString(medianed));
        System.out.println(java.util.Arrays.deepToString(sobel));
        System.out.println(java.util.Arrays.deepToString(lbp_img));
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
