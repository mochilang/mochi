public class Main {
    static double[][] coefficient;
    static double[] constant;
    static double[] init_val;
    static int iterations;
    static double[] result;

    static double absf(double x) {
        return x < 0.0 ? -x : x;
    }

    static boolean strictly_diagonally_dominant(double[][] matrix) {
        int n = matrix.length;
        int i = 0;
        while (i < n) {
            double sum = 0.0;
            int j = 0;
            while (j < n) {
                if (i != j) {
                    sum = sum + absf(matrix[i][j]);
                }
                j = j + 1;
            }
            if (absf(matrix[i][i]) <= sum) {
                throw new RuntimeException(String.valueOf("Coefficient matrix is not strictly diagonally dominant"));
            }
            i = i + 1;
        }
        return true;
    }

    static double[] jacobi_iteration_method(double[][] coefficient, double[] constant, double[] init_val, int iterations) {
        int n_1 = coefficient.length;
        if (n_1 == 0) {
            throw new RuntimeException(String.valueOf("Coefficient matrix cannot be empty"));
        }
        if (constant.length != n_1) {
            throw new RuntimeException(String.valueOf("Constant vector length must equal number of rows in coefficient matrix"));
        }
        if (init_val.length != n_1) {
            throw new RuntimeException(String.valueOf("Initial values count must match matrix size"));
        }
        int r = 0;
        while (r < n_1) {
            if (coefficient[r].length != n_1) {
                throw new RuntimeException(String.valueOf("Coefficient matrix must be square"));
            }
            r = r + 1;
        }
        if (iterations <= 0) {
            throw new RuntimeException(String.valueOf("Iterations must be at least 1"));
        }
        strictly_diagonally_dominant(((double[][])(coefficient)));
        double[] x = ((double[])(init_val));
        int k = 0;
        while (k < iterations) {
            double[] new_x = ((double[])(new double[]{}));
            int i_1 = 0;
            while (i_1 < n_1) {
                double sum_1 = 0.0;
                int j_1 = 0;
                while (j_1 < n_1) {
                    if (i_1 != j_1) {
                        sum_1 = sum_1 + coefficient[i_1][j_1] * x[j_1];
                    }
                    j_1 = j_1 + 1;
                }
                double value = (constant[i_1] - sum_1) / coefficient[i_1][i_1];
                new_x = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(new_x), java.util.stream.DoubleStream.of(value)).toArray()));
                i_1 = i_1 + 1;
            }
            x = ((double[])(new_x));
            k = k + 1;
        }
        return x;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            coefficient = ((double[][])(new double[][]{new double[]{4.0, 1.0, 1.0}, new double[]{1.0, 5.0, 2.0}, new double[]{1.0, 2.0, 4.0}}));
            constant = ((double[])(new double[]{2.0, -6.0, -4.0}));
            init_val = ((double[])(new double[]{0.5, -0.5, -0.5}));
            iterations = 3;
            result = ((double[])(jacobi_iteration_method(((double[][])(coefficient)), ((double[])(constant)), ((double[])(init_val)), iterations)));
            System.out.println(java.util.Arrays.toString(result));
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
