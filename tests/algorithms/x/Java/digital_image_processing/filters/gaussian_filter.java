public class Main {
    static double PI;
    static int[][] img;
    static int[][] gaussian3;
    static int[][] gaussian5;

    static double expApprox(double x) {
        double sum = 1.0;
        double term = 1.0;
        int n = 1;
        while (n < 10) {
            term = term * x / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double[][] gen_gaussian_kernel(int k_size, double sigma) {
        int center = k_size / 2;
        double[][] kernel = ((double[][])(new double[][]{}));
        int i = 0;
        while (i < k_size) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < k_size) {
                double x = ((Number)((i - center))).doubleValue();
                double y = ((Number)((j - center))).doubleValue();
                double exponent = -((x * x + y * y) / (2.0 * sigma * sigma));
                double value = (1.0 / (2.0 * PI * sigma)) * expApprox(exponent);
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(value)).toArray()));
                j = j + 1;
            }
            kernel = ((double[][])(appendObj(kernel, row)));
            i = i + 1;
        }
        return kernel;
    }

    static int[][] gaussian_filter(int[][] image, int k_size, double sigma) {
        int height = image.length;
        int width = image[0].length;
        int dst_height = height - k_size + 1;
        int dst_width = width - k_size + 1;
        double[][] kernel_1 = ((double[][])(gen_gaussian_kernel(k_size, sigma)));
        int[][] dst = ((int[][])(new int[][]{}));
        int i_1 = 0;
        while (i_1 < dst_height) {
            int[] row_1 = ((int[])(new int[]{}));
            int j_1 = 0;
            while (j_1 < dst_width) {
                double sum_1 = 0.0;
                int ki = 0;
                while (ki < k_size) {
                    int kj = 0;
                    while (kj < k_size) {
                        sum_1 = sum_1 + (((double)(image[i_1 + ki][j_1 + kj]))) * kernel_1[ki][kj];
                        kj = kj + 1;
                    }
                    ki = ki + 1;
                }
                row_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(row_1), java.util.stream.IntStream.of(((Number)(sum_1)).intValue())).toArray()));
                j_1 = j_1 + 1;
            }
            dst = ((int[][])(appendObj(dst, row_1)));
            i_1 = i_1 + 1;
        }
        return dst;
    }

    static void print_image(int[][] image) {
        int i_2 = 0;
        while (i_2 < image.length) {
            System.out.println(java.util.Arrays.toString(image[i_2]));
            i_2 = i_2 + 1;
        }
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            img = ((int[][])(new int[][]{new int[]{52, 55, 61, 59, 79}, new int[]{62, 59, 55, 104, 94}, new int[]{63, 65, 66, 113, 144}, new int[]{68, 70, 70, 126, 154}, new int[]{70, 72, 69, 128, 155}}));
            gaussian3 = ((int[][])(gaussian_filter(((int[][])(img)), 3, 1.0)));
            gaussian5 = ((int[][])(gaussian_filter(((int[][])(img)), 5, 0.8)));
            print_image(((int[][])(gaussian3)));
            print_image(((int[][])(gaussian5)));
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
