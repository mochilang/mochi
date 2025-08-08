public class Main {
    static double PI;
    static double[][] img;
    static double result;

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double expApprox(double x) {
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 10) {
            term = term * x / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        return sum;
    }

    static double[][] vec_gaussian(double[][] mat, double variance) {
        int i_1 = 0;
        double[][] out = ((double[][])(new double[][]{}));
        while (i_1 < mat.length) {
            double[] row = ((double[])(new double[]{}));
            int j = 0;
            while (j < mat[i_1].length) {
                double v = mat[i_1][j];
                double e = -(v * v) / (2.0 * variance);
                row = ((double[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row), java.util.Arrays.stream(new double[]{expApprox(e)})).toArray(double[]::new)));
                j = j + 1;
            }
            out = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(out), java.util.Arrays.stream(new double[][]{row})).toArray(double[][]::new)));
            i_1 = i_1 + 1;
        }
        return out;
    }

    static double[][] get_slice(double[][] img, int x, int y, int kernel_size) {
        int half = Math.floorDiv(kernel_size, 2);
        int i_2 = x - half;
        double[][] slice = ((double[][])(new double[][]{}));
        while (i_2 <= x + half) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = y - half;
            while (j_1 <= y + half) {
                row_1 = ((double[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_1), java.util.Arrays.stream(new double[]{img[i_2][j_1]})).toArray(double[]::new)));
                j_1 = j_1 + 1;
            }
            slice = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(slice), java.util.Arrays.stream(new double[][]{row_1})).toArray(double[][]::new)));
            i_2 = i_2 + 1;
        }
        return slice;
    }

    static double[][] get_gauss_kernel(int kernel_size, double spatial_variance) {
        double[][] arr = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < kernel_size) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_2 = 0;
            while (j_2 < kernel_size) {
                double di = ((Number)((i_3 - Math.floorDiv(kernel_size, 2)))).doubleValue();
                double dj = ((Number)((j_2 - Math.floorDiv(kernel_size, 2)))).doubleValue();
                double dist = sqrtApprox(di * di + dj * dj);
                row_2 = ((double[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_2), java.util.Arrays.stream(new double[]{dist})).toArray(double[]::new)));
                j_2 = j_2 + 1;
            }
            arr = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(arr), java.util.Arrays.stream(new double[][]{row_2})).toArray(double[][]::new)));
            i_3 = i_3 + 1;
        }
        return vec_gaussian(((double[][])(arr)), spatial_variance);
    }

    static double[][] elementwise_sub(double[][] mat, double value) {
        double[][] res = ((double[][])(new double[][]{}));
        int i_4 = 0;
        while (i_4 < mat.length) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_3 = 0;
            while (j_3 < mat[i_4].length) {
                row_3 = ((double[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_3), java.util.Arrays.stream(new double[]{mat[i_4][j_3] - value})).toArray(double[]::new)));
                j_3 = j_3 + 1;
            }
            res = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.Arrays.stream(new double[][]{row_3})).toArray(double[][]::new)));
            i_4 = i_4 + 1;
        }
        return res;
    }

    static double[][] elementwise_mul(double[][] a, double[][] b) {
        double[][] res_1 = ((double[][])(new double[][]{}));
        int i_5 = 0;
        while (i_5 < a.length) {
            double[] row_4 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < a[i_5].length) {
                row_4 = ((double[])(java.util.stream.Stream.concat(java.util.Arrays.stream(row_4), java.util.Arrays.stream(new double[]{a[i_5][j_4] * b[i_5][j_4]})).toArray(double[]::new)));
                j_4 = j_4 + 1;
            }
            res_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(res_1), java.util.Arrays.stream(new double[][]{row_4})).toArray(double[][]::new)));
            i_5 = i_5 + 1;
        }
        return res_1;
    }

    static double matrix_sum(double[][] mat) {
        double total = 0.0;
        int i_6 = 0;
        while (i_6 < mat.length) {
            int j_5 = 0;
            while (j_5 < mat[i_6].length) {
                total = total + mat[i_6][j_5];
                j_5 = j_5 + 1;
            }
            i_6 = i_6 + 1;
        }
        return total;
    }

    static double bilateral_filter(double[][] img, double spatial_variance, double intensity_variance, int kernel_size) {
        double[][] gauss_ker = ((double[][])(get_gauss_kernel(kernel_size, spatial_variance)));
        double[][] img_s = ((double[][])(img));
        double center = img_s[Math.floorDiv(kernel_size, 2)][Math.floorDiv(kernel_size, 2)];
        double[][] img_i = ((double[][])(elementwise_sub(((double[][])(img_s)), center)));
        double[][] img_ig = ((double[][])(vec_gaussian(((double[][])(img_i)), intensity_variance)));
        double[][] weights = ((double[][])(elementwise_mul(((double[][])(gauss_ker)), ((double[][])(img_ig)))));
        double[][] vals = ((double[][])(elementwise_mul(((double[][])(img_s)), ((double[][])(weights)))));
        double sum_weights = matrix_sum(((double[][])(weights)));
        double val = 0.0;
        if (sum_weights != 0.0) {
            val = matrix_sum(((double[][])(vals))) / sum_weights;
        }
        return val;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            PI = 3.141592653589793;
            img = ((double[][])(new double[][]{new double[]{0.2, 0.3, 0.4}, new double[]{0.3, 0.4, 0.5}, new double[]{0.4, 0.5, 0.6}}));
            result = bilateral_filter(((double[][])(img)), 1.0, 1.0, 3);
            System.out.println(result);
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
}
