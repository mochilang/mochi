public class Main {
    static class PCAResult {
        double[][] transformed;
        double[] variance_ratio;
        PCAResult(double[][] transformed, double[] variance_ratio) {
            this.transformed = transformed;
            this.variance_ratio = variance_ratio;
        }
        PCAResult() {}
        @Override public String toString() {
            return String.format("{'transformed': %s, 'variance_ratio': %s}", String.valueOf(transformed), String.valueOf(variance_ratio));
        }
    }

    static class Eigen {
        double[] values;
        double[][] vectors;
        Eigen(double[] values, double[][] vectors) {
            this.values = values;
            this.vectors = vectors;
        }
        Eigen() {}
        @Override public String toString() {
            return String.format("{'values': %s, 'vectors': %s}", String.valueOf(values), String.valueOf(vectors));
        }
    }

    static double[][] data;
    static PCAResult result_1;
    static int idx = 0;

    static double sqrt(double x) {
        double guess = x > 1.0 ? x / 2.0 : 1.0;
        int i = 0;
        while (i < 20) {
            guess = 0.5 * (guess + x / guess);
            i = i + 1;
        }
        return guess;
    }

    static double mean(double[] xs) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < xs.length) {
            sum = sum + xs[i_1];
            i_1 = i_1 + 1;
        }
        return sum / xs.length;
    }

    static double[][] standardize(double[][] data) {
        int n_samples = data.length;
        int n_features = data[0].length;
        double[] means = ((double[])(new double[]{}));
        double[] stds = ((double[])(new double[]{}));
        int j = 0;
        while (j < n_features) {
            double[] column = ((double[])(new double[]{}));
            int i_2 = 0;
            while (i_2 < n_samples) {
                column = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(column), java.util.stream.DoubleStream.of(data[i_2][j])).toArray()));
                i_2 = i_2 + 1;
            }
            double m = mean(((double[])(column)));
            means = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(means), java.util.stream.DoubleStream.of(m)).toArray()));
            double variance = 0.0;
            int k = 0;
            while (k < n_samples) {
                double diff = column[k] - m;
                variance = variance + diff * diff;
                k = k + 1;
            }
            stds = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stds), java.util.stream.DoubleStream.of(sqrt(variance / (n_samples - 1)))).toArray()));
            j = j + 1;
        }
        double[][] standardized = ((double[][])(new double[][]{}));
        int r = 0;
        while (r < n_samples) {
            double[] row = ((double[])(new double[]{}));
            int c = 0;
            while (c < n_features) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of((data[r][c] - means[c]) / stds[c])).toArray()));
                c = c + 1;
            }
            standardized = ((double[][])(appendObj(standardized, row)));
            r = r + 1;
        }
        return standardized;
    }

    static double[][] covariance_matrix(double[][] data) {
        int n_samples_1 = data.length;
        int n_features_1 = data[0].length;
        double[][] cov = ((double[][])(new double[][]{}));
        int i_3 = 0;
        while (i_3 < n_features_1) {
            double[] row_1 = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < n_features_1) {
                double sum_1 = 0.0;
                int k_1 = 0;
                while (k_1 < n_samples_1) {
                    sum_1 = sum_1 + data[k_1][i_3] * data[k_1][j_1];
                    k_1 = k_1 + 1;
                }
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(sum_1 / (n_samples_1 - 1))).toArray()));
                j_1 = j_1 + 1;
            }
            cov = ((double[][])(appendObj(cov, row_1)));
            i_3 = i_3 + 1;
        }
        return cov;
    }

    static double[] normalize(double[] vec) {
        double sum_2 = 0.0;
        int i_4 = 0;
        while (i_4 < vec.length) {
            sum_2 = sum_2 + vec[i_4] * vec[i_4];
            i_4 = i_4 + 1;
        }
        double n = sqrt(sum_2);
        double[] res = ((double[])(new double[]{}));
        int j_2 = 0;
        while (j_2 < vec.length) {
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(vec[j_2] / n)).toArray()));
            j_2 = j_2 + 1;
        }
        return res;
    }

    static Eigen eigen_decomposition_2x2(double[][] matrix) {
        double a = matrix[0][0];
        double b = matrix[0][1];
        double c_1 = matrix[1][1];
        double diff_1 = a - c_1;
        double discriminant = sqrt(diff_1 * diff_1 + 4.0 * b * b);
        double lambda1 = (a + c_1 + discriminant) / 2.0;
        double lambda2 = (a + c_1 - discriminant) / 2.0;
        double[] v1 = new double[0];
        double[] v2 = new double[0];
        if (b != 0.0) {
            v1 = ((double[])(normalize(((double[])(new Object[]{lambda1 - c_1, b})))));
            v2 = ((double[])(normalize(((double[])(new Object[]{lambda2 - c_1, b})))));
        } else {
            v1 = ((double[])(new double[]{1.0, 0.0}));
            v2 = ((double[])(new double[]{0.0, 1.0}));
        }
        double[] eigenvalues = ((double[])(new double[]{lambda1, lambda2}));
        double[][] eigenvectors = ((double[][])(new double[][]{v1, v2}));
        if (eigenvalues[0] < eigenvalues[1]) {
            double tmp_val = eigenvalues[0];
eigenvalues[0] = eigenvalues[1];
eigenvalues[1] = tmp_val;
            double[] tmp_vec = ((double[])(eigenvectors[0]));
eigenvectors[0] = ((double[])(eigenvectors[1]));
eigenvectors[1] = ((double[])(tmp_vec));
        }
        return new Eigen(eigenvalues, eigenvectors);
    }

    static double[][] transpose(double[][] matrix) {
        int rows = matrix.length;
        int cols = matrix[0].length;
        double[][] trans = ((double[][])(new double[][]{}));
        int i_5 = 0;
        while (i_5 < cols) {
            double[] row_2 = ((double[])(new double[]{}));
            int j_3 = 0;
            while (j_3 < rows) {
                row_2 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_2), java.util.stream.DoubleStream.of(matrix[j_3][i_5])).toArray()));
                j_3 = j_3 + 1;
            }
            trans = ((double[][])(appendObj(trans, row_2)));
            i_5 = i_5 + 1;
        }
        return trans;
    }

    static double[][] matrix_multiply(double[][] a, double[][] b) {
        int rows_a = a.length;
        int cols_a = a[0].length;
        int rows_b = b.length;
        int cols_b = b[0].length;
        if (cols_a != rows_b) {
            throw new RuntimeException(String.valueOf("Incompatible matrices"));
        }
        double[][] result = ((double[][])(new double[][]{}));
        int i_6 = 0;
        while (i_6 < rows_a) {
            double[] row_3 = ((double[])(new double[]{}));
            int j_4 = 0;
            while (j_4 < cols_b) {
                double sum_3 = 0.0;
                int k_2 = 0;
                while (k_2 < cols_a) {
                    sum_3 = sum_3 + a[i_6][k_2] * b[k_2][j_4];
                    k_2 = k_2 + 1;
                }
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(sum_3)).toArray()));
                j_4 = j_4 + 1;
            }
            result = ((double[][])(appendObj(result, row_3)));
            i_6 = i_6 + 1;
        }
        return result;
    }

    static PCAResult apply_pca(double[][] data, int n_components) {
        double[][] standardized_1 = ((double[][])(standardize(((double[][])(data)))));
        double[][] cov_1 = ((double[][])(covariance_matrix(((double[][])(standardized_1)))));
        Eigen eig = eigen_decomposition_2x2(((double[][])(cov_1)));
        double[] eigenvalues_1 = ((double[])(eig.values));
        double[][] eigenvectors_1 = ((double[][])(eig.vectors));
        double[][] components = ((double[][])(transpose(((double[][])(eigenvectors_1)))));
        double[][] transformed = ((double[][])(matrix_multiply(((double[][])(standardized_1)), ((double[][])(components)))));
        double total = eigenvalues_1[0] + eigenvalues_1[1];
        double[] ratios = ((double[])(new double[]{}));
        int i_7 = 0;
        while (i_7 < n_components) {
            ratios = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ratios), java.util.stream.DoubleStream.of(eigenvalues_1[i_7] / total)).toArray()));
            i_7 = i_7 + 1;
        }
        return new PCAResult(transformed, ratios);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            data = ((double[][])(new double[][]{new double[]{2.5, 2.4}, new double[]{0.5, 0.7}, new double[]{2.2, 2.9}, new double[]{1.9, 2.2}, new double[]{3.1, 3.0}, new double[]{2.3, 2.7}, new double[]{2.0, 1.6}, new double[]{1.0, 1.1}, new double[]{1.5, 1.6}, new double[]{1.1, 0.9}}));
            result_1 = apply_pca(((double[][])(data)), 2);
            System.out.println("Transformed Data (first 5 rows):");
            idx = 0;
            while (idx < 5) {
                System.out.println(java.util.Arrays.toString(result_1.transformed[idx]));
                idx = idx + 1;
            }
            System.out.println("Explained Variance Ratio:");
            System.out.println(java.util.Arrays.toString(result_1.variance_ratio));
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
