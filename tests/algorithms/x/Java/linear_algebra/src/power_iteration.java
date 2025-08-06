public class Main {
    static class PowerResult {
        double eigenvalue;
        double[] eigenvector;
        PowerResult(double eigenvalue, double[] eigenvector) {
            this.eigenvalue = eigenvalue;
            this.eigenvector = eigenvector;
        }
        PowerResult() {}
        @Override public String toString() {
            return String.format("{'eigenvalue': %s, 'eigenvector': %s}", String.valueOf(eigenvalue), String.valueOf(eigenvector));
        }
    }

    static double[][] input_matrix;
    static double[] vector;
    static PowerResult result;

    static double abs(double x) {
        if (x < 0.0) {
            return -x;
        }
        return x;
    }

    static double sqrtApprox(double x) {
        if (x == 0.0) {
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

    static double dot(double[] a, double[] b) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < a.length) {
            sum = sum + a[i_1] * b[i_1];
            i_1 = i_1 + 1;
        }
        return sum;
    }

    static double[] mat_vec_mult(double[][] mat, double[] vec) {
        double[] res = ((double[])(new double[]{}));
        int i_2 = 0;
        while (i_2 < mat.length) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(dot(((double[])(mat[i_2])), ((double[])(vec))))).toArray()));
            i_2 = i_2 + 1;
        }
        return res;
    }

    static double norm(double[] vec) {
        double sum_1 = 0.0;
        int i_3 = 0;
        while (i_3 < vec.length) {
            sum_1 = sum_1 + vec[i_3] * vec[i_3];
            i_3 = i_3 + 1;
        }
        double root = sqrtApprox(sum_1);
        return root;
    }

    static double[] normalize(double[] vec) {
        double n = norm(((double[])(vec)));
        double[] res_1 = ((double[])(new double[]{}));
        int i_4 = 0;
        while (i_4 < vec.length) {
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(vec[i_4] / n)).toArray()));
            i_4 = i_4 + 1;
        }
        return res_1;
    }

    static PowerResult power_iteration(double[][] matrix, double[] vector, double error_tol, int max_iterations) {
        double[] v = ((double[])(normalize(((double[])(vector)))));
        double lambda_prev = 0.0;
        double lambda = 0.0;
        double err = 1000000000000.0;
        int iterations = 0;
        while (err > error_tol && iterations < max_iterations) {
            double[] w = ((double[])(mat_vec_mult(((double[][])(matrix)), ((double[])(v)))));
            v = ((double[])(normalize(((double[])(w)))));
            double[] mv = ((double[])(mat_vec_mult(((double[][])(matrix)), ((double[])(v)))));
            lambda = dot(((double[])(v)), ((double[])(mv)));
            double denom = lambda != 0.0 ? Math.abs(lambda) : 1.0;
            err = Math.abs(lambda - lambda_prev) / denom;
            lambda_prev = lambda;
            iterations = iterations + 1;
        }
        return new PowerResult(lambda, v);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            input_matrix = ((double[][])(new double[][]{new double[]{41.0, 4.0, 20.0}, new double[]{4.0, 26.0, 30.0}, new double[]{20.0, 30.0, 50.0}}));
            vector = ((double[])(new double[]{41.0, 4.0, 20.0}));
            result = power_iteration(((double[][])(input_matrix)), ((double[])(vector)), 1e-12, 100);
            System.out.println(_p(result.eigenvalue));
            System.out.println(_p(result.eigenvector));
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
