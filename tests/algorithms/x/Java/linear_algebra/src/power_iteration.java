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
        double guess_1 = x / 2.0;
        long i_1 = 0L;
        while (i_1 < 20) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double dot(double[] a, double[] b) {
        double sum = 0.0;
        long i_3 = 0L;
        while (i_3 < a.length) {
            sum = sum + a[(int)((long)(i_3))] * b[(int)((long)(i_3))];
            i_3 = i_3 + 1;
        }
        return sum;
    }

    static double[] mat_vec_mult(double[][] mat, double[] vec) {
        double[] res = ((double[])(new double[]{}));
        long i_5 = 0L;
        while (i_5 < mat.length) {
            res = ((double[])(appendDouble(res, dot(((double[])(mat[(int)((long)(i_5))])), ((double[])(vec))))));
            i_5 = i_5 + 1;
        }
        return res;
    }

    static double norm(double[] vec) {
        double sum_1 = 0.0;
        long i_7 = 0L;
        while (i_7 < vec.length) {
            sum_1 = sum_1 + vec[(int)((long)(i_7))] * vec[(int)((long)(i_7))];
            i_7 = i_7 + 1;
        }
        double root_1 = sqrtApprox(sum_1);
        return root_1;
    }

    static double[] normalize(double[] vec) {
        double n = norm(((double[])(vec)));
        double[] res_2 = ((double[])(new double[]{}));
        long i_9 = 0L;
        while (i_9 < vec.length) {
            res_2 = ((double[])(appendDouble(res_2, vec[(int)((long)(i_9))] / n)));
            i_9 = i_9 + 1;
        }
        return res_2;
    }

    static PowerResult power_iteration(double[][] matrix, double[] vector, double error_tol, long max_iterations) {
        double[] v = ((double[])(normalize(((double[])(vector)))));
        double lambda_prev_1 = 0.0;
        double lambda_1 = 0.0;
        double err_1 = 1000000000000.0;
        long iterations_1 = 0L;
        while (err_1 > error_tol && iterations_1 < max_iterations) {
            double[] w_1 = ((double[])(mat_vec_mult(((double[][])(matrix)), ((double[])(v)))));
            v = ((double[])(normalize(((double[])(w_1)))));
            double[] mv_1 = ((double[])(mat_vec_mult(((double[][])(matrix)), ((double[])(v)))));
            lambda_1 = dot(((double[])(v)), ((double[])(mv_1)));
            double denom_1 = lambda_1 != 0.0 ? Math.abs(lambda_1) : 1.0;
            err_1 = Math.abs(lambda_1 - lambda_prev_1) / denom_1;
            lambda_prev_1 = lambda_1;
            iterations_1 = iterations_1 + 1;
        }
        return new PowerResult(lambda_1, v);
    }
    public static void main(String[] args) {
        input_matrix = ((double[][])(new double[][]{new double[]{41.0, 4.0, 20.0}, new double[]{4.0, 26.0, 30.0}, new double[]{20.0, 30.0, 50.0}}));
        vector = ((double[])(new double[]{41.0, 4.0, 20.0}));
        result = power_iteration(((double[][])(input_matrix)), ((double[])(vector)), 1e-12, 100L);
        System.out.println(_p(result.eigenvalue));
        System.out.println(_p(result.eigenvector));
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
