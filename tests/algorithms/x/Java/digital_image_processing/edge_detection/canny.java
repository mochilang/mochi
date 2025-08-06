public class Main {
    static double PI;
    static double[][] GAUSSIAN_KERNEL;
    static double[][] SOBEL_GX;
    static double[][] SOBEL_GY;
    static double[][] image;
    static double[][] edges;

    static double sqrtApprox(double x) {
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
        if (x > 0.0) {
            double r = atanApprox(y / x);
            return r;
        }
        if (x < 0.0) {
            if (y >= 0.0) {
                return atanApprox(y / x) + PI;
            }
            return atanApprox(y / x) - PI;
        }
        if (y > 0.0) {
            return PI / 2.0;
        }
        if (y < 0.0) {
            return -PI / 2.0;
        }
        return 0.0;
    }

    static double deg(double rad) {
        return rad * 180.0 / PI;
    }

    static double[][] zero_matrix(int h, int w) {
        double[][] out = ((double[][])(new double[][]{}));
        int i_1 = 0;
        while (i_1 < h) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < w) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
                j = j + 1;
            }
            out = ((double[][])(appendObj(out, row)));
            i_1 = i_1 + 1;
        }
        return out;
    }

    static double[][] convolve(double[][] img, double[][] kernel) {
        int h = img.length;
        int w = img[0].length;
        int k = kernel.length;
        int pad = k / 2;
        double[][] out_1 = ((double[][])(zero_matrix(h, w)));
        int y = pad;
        while (y < h - pad) {
            int x = pad;
            while (x < w - pad) {
                double sum = 0.0;
                int ky = 0;
                while (ky < k) {
                    int kx = 0;
                    while (kx < k) {
                        double pixel = img[y - pad + ky][x - pad + kx];
                        double weight = kernel[ky][kx];
                        sum = sum + pixel * weight;
                        kx = kx + 1;
                    }
                    ky = ky + 1;
                }
out_1[y][x] = sum;
                x = x + 1;
            }
            y = y + 1;
        }
        return out_1;
    }

    static double[][] gaussian_blur(double[][] img) {
        return convolve(((double[][])(img)), ((double[][])(GAUSSIAN_KERNEL)));
    }

    static java.util.Map<String,double[][]> sobel_filter(double[][] img) {
        double[][] gx = ((double[][])(convolve(((double[][])(img)), ((double[][])(SOBEL_GX)))));
        double[][] gy = ((double[][])(convolve(((double[][])(img)), ((double[][])(SOBEL_GY)))));
        int h_1 = img.length;
        int w_1 = img[0].length;
        double[][] grad = ((double[][])(zero_matrix(h_1, w_1)));
        double[][] dir = ((double[][])(zero_matrix(h_1, w_1)));
        int i_2 = 0;
        while (i_2 < h_1) {
            int j_1 = 0;
            while (j_1 < w_1) {
                double gxx = gx[i_2][j_1];
                double gyy = gy[i_2][j_1];
grad[i_2][j_1] = sqrtApprox(gxx * gxx + gyy * gyy);
dir[i_2][j_1] = deg(atan2Approx(gyy, gxx)) + 180.0;
                j_1 = j_1 + 1;
            }
            i_2 = i_2 + 1;
        }
        return new java.util.LinkedHashMap<String, double[][]>(java.util.Map.ofEntries(java.util.Map.entry("grad", ((double[][])(grad))), java.util.Map.entry("dir", ((double[][])(dir)))));
    }

    static double[][] suppress_non_maximum(int h, int w, double[][] direction, double[][] grad) {
        double[][] dest = ((double[][])(zero_matrix(h, w)));
        int r_1 = 1;
        while (r_1 < h - 1) {
            int c = 1;
            while (c < w - 1) {
                double angle = direction[r_1][c];
                double q = 0.0;
                double p = 0.0;
                if ((angle >= 0.0 && angle < 22.5) || (angle >= 157.5 && angle <= 180.0) || (angle >= 337.5)) {
                    q = grad[r_1][c + 1];
                    p = grad[r_1][c - 1];
                } else                 if ((angle >= 22.5 && angle < 67.5) || (angle >= 202.5 && angle < 247.5)) {
                    q = grad[r_1 + 1][c - 1];
                    p = grad[r_1 - 1][c + 1];
                } else                 if ((angle >= 67.5 && angle < 112.5) || (angle >= 247.5 && angle < 292.5)) {
                    q = grad[r_1 + 1][c];
                    p = grad[r_1 - 1][c];
                } else {
                    q = grad[r_1 - 1][c - 1];
                    p = grad[r_1 + 1][c + 1];
                }
                if (grad[r_1][c] >= q && grad[r_1][c] >= p) {
dest[r_1][c] = grad[r_1][c];
                }
                c = c + 1;
            }
            r_1 = r_1 + 1;
        }
        return dest;
    }

    static void double_threshold(int h, int w, double[][] img, double low, double high, double weak, double strong) {
        int r_2 = 0;
        while (r_2 < h) {
            int c_1 = 0;
            while (c_1 < w) {
                double v = img[r_2][c_1];
                if (v >= high) {
img[r_2][c_1] = strong;
                } else                 if (v < low) {
img[r_2][c_1] = 0.0;
                } else {
img[r_2][c_1] = weak;
                }
                c_1 = c_1 + 1;
            }
            r_2 = r_2 + 1;
        }
    }

    static void track_edge(int h, int w, double[][] img, double weak, double strong) {
        int r_3 = 1;
        while (r_3 < h - 1) {
            int c_2 = 1;
            while (c_2 < w - 1) {
                if (img[r_3][c_2] == weak) {
                    if (img[r_3 + 1][c_2] == strong || img[r_3 - 1][c_2] == strong || img[r_3][c_2 + 1] == strong || img[r_3][c_2 - 1] == strong || img[r_3 - 1][c_2 - 1] == strong || img[r_3 - 1][c_2 + 1] == strong || img[r_3 + 1][c_2 - 1] == strong || img[r_3 + 1][c_2 + 1] == strong) {
img[r_3][c_2] = strong;
                    } else {
img[r_3][c_2] = 0.0;
                    }
                }
                c_2 = c_2 + 1;
            }
            r_3 = r_3 + 1;
        }
    }

    static double[][] canny(double[][] image, double low, double high, double weak, double strong) {
        double[][] blurred = ((double[][])(gaussian_blur(((double[][])(image)))));
        java.util.Map<String,double[][]> sob = sobel_filter(((double[][])(blurred)));
        double[][] grad_1 = (double[][])(((double[][])(sob).get("grad")));
        double[][] direction = (double[][])(((double[][])(sob).get("dir")));
        int h_2 = image.length;
        int w_2 = image[0].length;
        double[][] suppressed = ((double[][])(suppress_non_maximum(h_2, w_2, ((double[][])(direction)), ((double[][])(grad_1)))));
        double_threshold(h_2, w_2, ((double[][])(suppressed)), low, high, weak, strong);
        track_edge(h_2, w_2, ((double[][])(suppressed)), weak, strong);
        return suppressed;
    }

    static void print_image(double[][] img) {
        int r_4 = 0;
        while (r_4 < img.length) {
            int c_3 = 0;
            String line = "";
            while (c_3 < img[r_4].length) {
                line = line + _p(_geto(img[r_4], c_3)) + " ";
                c_3 = c_3 + 1;
            }
            System.out.println(line);
            r_4 = r_4 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            GAUSSIAN_KERNEL = ((double[][])(new double[][]{new double[]{0.0625, 0.125, 0.0625}, new double[]{0.125, 0.25, 0.125}, new double[]{0.0625, 0.125, 0.0625}}));
            SOBEL_GX = ((double[][])(new double[][]{new double[]{-1.0, 0.0, 1.0}, new double[]{-2.0, 0.0, 2.0}, new double[]{-1.0, 0.0, 1.0}}));
            SOBEL_GY = ((double[][])(new double[][]{new double[]{1.0, 2.0, 1.0}, new double[]{0.0, 0.0, 0.0}, new double[]{-1.0, -2.0, -1.0}}));
            image = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0, 0.0, 0.0}, new double[]{0.0, 255.0, 255.0, 255.0, 0.0}, new double[]{0.0, 255.0, 255.0, 255.0, 0.0}, new double[]{0.0, 255.0, 255.0, 255.0, 0.0}, new double[]{0.0, 0.0, 0.0, 0.0, 0.0}}));
            edges = ((double[][])(canny(((double[][])(image)), 20.0, 40.0, 128.0, 255.0)));
            print_image(((double[][])(edges)));
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
