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

    static double[][] data = ((double[][])(new double[][]{new double[]{2.5, 2.4}, new double[]{0.5, 0.7}, new double[]{2.2, 2.9}, new double[]{1.9, 2.2}, new double[]{3.1, 3.0}, new double[]{2.3, 2.7}, new double[]{2.0, 1.6}, new double[]{1.0, 1.1}, new double[]{1.5, 1.6}, new double[]{1.1, 0.9}}));
    static PCAResult result_2;
    static long idx = 0L;

    static double sqrt(double x) {
        double guess = (double)((double)(x) > (double)(1.0) ? (double)(x) / (double)(2.0) : 1.0);
        long i_1 = 0L;
        while ((long)(i_1) < 20L) {
            guess = (double)((double)(0.5) * (double)(((double)(guess) + (double)((double)(x) / (double)(guess)))));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess;
    }

    static double mean(double[] xs) {
        double sum = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(xs.length)) {
            sum = (double)((double)(sum) + (double)(xs[(int)((long)(i_3))]));
            i_3 = (long)((long)(i_3) + 1L);
        }
        return (double)(sum) / (double)(xs.length);
    }

    static double[][] standardize(double[][] data) {
        long n_samples = (long)(data.length);
        long n_features_1 = (long)(data[(int)(0L)].length);
        double[] means_1 = ((double[])(new double[]{}));
        double[] stds_1 = ((double[])(new double[]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(n_features_1)) {
            double[] column_1 = ((double[])(new double[]{}));
            long i_5 = 0L;
            while ((long)(i_5) < (long)(n_samples)) {
                column_1 = ((double[])(appendDouble(column_1, (double)(data[(int)((long)(i_5))][(int)((long)(j_1))]))));
                i_5 = (long)((long)(i_5) + 1L);
            }
            double m_1 = (double)(mean(((double[])(column_1))));
            means_1 = ((double[])(appendDouble(means_1, (double)(m_1))));
            double variance_1 = (double)(0.0);
            long k_1 = 0L;
            while ((long)(k_1) < (long)(n_samples)) {
                double diff_1 = (double)((double)(column_1[(int)((long)(k_1))]) - (double)(m_1));
                variance_1 = (double)((double)(variance_1) + (double)((double)(diff_1) * (double)(diff_1)));
                k_1 = (long)((long)(k_1) + 1L);
            }
            stds_1 = ((double[])(appendDouble(stds_1, (double)(sqrt((double)((double)(variance_1) / (double)(((long)(n_samples) - 1L))))))));
            j_1 = (long)((long)(j_1) + 1L);
        }
        double[][] standardized_1 = ((double[][])(new double[][]{}));
        long r_1 = 0L;
        while ((long)(r_1) < (long)(n_samples)) {
            double[] row_1 = ((double[])(new double[]{}));
            long c_1 = 0L;
            while ((long)(c_1) < (long)(n_features_1)) {
                row_1 = ((double[])(appendDouble(row_1, (double)((double)(((double)(data[(int)((long)(r_1))][(int)((long)(c_1))]) - (double)(means_1[(int)((long)(c_1))]))) / (double)(stds_1[(int)((long)(c_1))])))));
                c_1 = (long)((long)(c_1) + 1L);
            }
            standardized_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(standardized_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            r_1 = (long)((long)(r_1) + 1L);
        }
        return standardized_1;
    }

    static double[][] covariance_matrix(double[][] data) {
        long n_samples_1 = (long)(data.length);
        long n_features_3 = (long)(data[(int)(0L)].length);
        double[][] cov_1 = ((double[][])(new double[][]{}));
        long i_7 = 0L;
        while ((long)(i_7) < (long)(n_features_3)) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0L;
            while ((long)(j_3) < (long)(n_features_3)) {
                double sum_2 = (double)(0.0);
                long k_3 = 0L;
                while ((long)(k_3) < (long)(n_samples_1)) {
                    sum_2 = (double)((double)(sum_2) + (double)((double)(data[(int)((long)(k_3))][(int)((long)(i_7))]) * (double)(data[(int)((long)(k_3))][(int)((long)(j_3))])));
                    k_3 = (long)((long)(k_3) + 1L);
                }
                row_3 = ((double[])(appendDouble(row_3, (double)((double)(sum_2) / (double)(((long)(n_samples_1) - 1L))))));
                j_3 = (long)((long)(j_3) + 1L);
            }
            cov_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(cov_1), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
            i_7 = (long)((long)(i_7) + 1L);
        }
        return cov_1;
    }

    static double[] normalize(double[] vec) {
        double sum_3 = (double)(0.0);
        long i_9 = 0L;
        while ((long)(i_9) < (long)(vec.length)) {
            sum_3 = (double)((double)(sum_3) + (double)((double)(vec[(int)((long)(i_9))]) * (double)(vec[(int)((long)(i_9))])));
            i_9 = (long)((long)(i_9) + 1L);
        }
        double n_1 = (double)(sqrt((double)(sum_3)));
        double[] res_1 = ((double[])(new double[]{}));
        long j_5 = 0L;
        while ((long)(j_5) < (long)(vec.length)) {
            res_1 = ((double[])(appendDouble(res_1, (double)((double)(vec[(int)((long)(j_5))]) / (double)(n_1)))));
            j_5 = (long)((long)(j_5) + 1L);
        }
        return res_1;
    }

    static Eigen eigen_decomposition_2x2(double[][] matrix) {
        double a = (double)(matrix[(int)(0L)][(int)(0L)]);
        double b_1 = (double)(matrix[(int)(0L)][(int)(1L)]);
        double c_3 = (double)(matrix[(int)(1L)][(int)(1L)]);
        double diff_3 = (double)((double)(a) - (double)(c_3));
        double discriminant_1 = (double)(sqrt((double)((double)((double)(diff_3) * (double)(diff_3)) + (double)((double)((double)(4.0) * (double)(b_1)) * (double)(b_1)))));
        double lambda1_1 = (double)((double)(((double)((double)(a) + (double)(c_3)) + (double)(discriminant_1))) / (double)(2.0));
        double lambda2_1 = (double)((double)(((double)((double)(a) + (double)(c_3)) - (double)(discriminant_1))) / (double)(2.0));
        double[] v1_1 = new double[0];
        double[] v2_1 = new double[0];
        if ((double)(b_1) != (double)(0.0)) {
            v1_1 = ((double[])(normalize(((double[])(new double[]{(double)(lambda1_1) - (double)(c_3), b_1})))));
            v2_1 = ((double[])(normalize(((double[])(new double[]{(double)(lambda2_1) - (double)(c_3), b_1})))));
        } else {
            v1_1 = ((double[])(new double[]{1.0, 0.0}));
            v2_1 = ((double[])(new double[]{0.0, 1.0}));
        }
        double[] eigenvalues_1 = ((double[])(new double[]{lambda1_1, lambda2_1}));
        double[][] eigenvectors_1 = ((double[][])(new double[][]{v1_1, v2_1}));
        if ((double)(eigenvalues_1[(int)(0L)]) < (double)(eigenvalues_1[(int)(1L)])) {
            double tmp_val_1 = (double)(eigenvalues_1[(int)(0L)]);
eigenvalues_1[(int)(0L)] = (double)(eigenvalues_1[(int)(1L)]);
eigenvalues_1[(int)(1L)] = (double)(tmp_val_1);
            double[] tmp_vec_1 = ((double[])(eigenvectors_1[(int)(0L)]));
eigenvectors_1[(int)(0L)] = ((double[])(eigenvectors_1[(int)(1L)]));
eigenvectors_1[(int)(1L)] = ((double[])(tmp_vec_1));
        }
        return new Eigen(eigenvalues_1, eigenvectors_1);
    }

    static double[][] transpose(double[][] matrix) {
        long rows = (long)(matrix.length);
        long cols_1 = (long)(matrix[(int)(0L)].length);
        double[][] trans_1 = ((double[][])(new double[][]{}));
        long i_11 = 0L;
        while ((long)(i_11) < (long)(cols_1)) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_7 = 0L;
            while ((long)(j_7) < (long)(rows)) {
                row_5 = ((double[])(appendDouble(row_5, (double)(matrix[(int)((long)(j_7))][(int)((long)(i_11))]))));
                j_7 = (long)((long)(j_7) + 1L);
            }
            trans_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(trans_1), java.util.stream.Stream.of(new double[][]{row_5})).toArray(double[][]::new)));
            i_11 = (long)((long)(i_11) + 1L);
        }
        return trans_1;
    }

    static double[][] matrix_multiply(double[][] a, double[][] b) {
        long rows_a = (long)(a.length);
        long cols_a_1 = (long)(a[(int)(0L)].length);
        long rows_b_1 = (long)(b.length);
        long cols_b_1 = (long)(b[(int)(0L)].length);
        if ((long)(cols_a_1) != (long)(rows_b_1)) {
            throw new RuntimeException(String.valueOf("Incompatible matrices"));
        }
        double[][] result_1 = ((double[][])(new double[][]{}));
        long i_13 = 0L;
        while ((long)(i_13) < (long)(rows_a)) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_9 = 0L;
            while ((long)(j_9) < (long)(cols_b_1)) {
                double sum_5 = (double)(0.0);
                long k_5 = 0L;
                while ((long)(k_5) < (long)(cols_a_1)) {
                    sum_5 = (double)((double)(sum_5) + (double)((double)(a[(int)((long)(i_13))][(int)((long)(k_5))]) * (double)(b[(int)((long)(k_5))][(int)((long)(j_9))])));
                    k_5 = (long)((long)(k_5) + 1L);
                }
                row_7 = ((double[])(appendDouble(row_7, (double)(sum_5))));
                j_9 = (long)((long)(j_9) + 1L);
            }
            result_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(new double[][]{row_7})).toArray(double[][]::new)));
            i_13 = (long)((long)(i_13) + 1L);
        }
        return result_1;
    }

    static PCAResult apply_pca(double[][] data, long n_components) {
        double[][] standardized_2 = ((double[][])(standardize(((double[][])(data)))));
        double[][] cov_3 = ((double[][])(covariance_matrix(((double[][])(standardized_2)))));
        Eigen eig_1 = eigen_decomposition_2x2(((double[][])(cov_3)));
        double[] eigenvalues_3 = ((double[])(eig_1.values));
        double[][] eigenvectors_3 = ((double[][])(eig_1.vectors));
        double[][] components_1 = ((double[][])(transpose(((double[][])(eigenvectors_3)))));
        double[][] transformed_1 = ((double[][])(matrix_multiply(((double[][])(standardized_2)), ((double[][])(components_1)))));
        double total_1 = (double)((double)(eigenvalues_3[(int)(0L)]) + (double)(eigenvalues_3[(int)(1L)]));
        double[] ratios_1 = ((double[])(new double[]{}));
        long i_15 = 0L;
        while ((long)(i_15) < (long)(n_components)) {
            ratios_1 = ((double[])(appendDouble(ratios_1, (double)((double)(eigenvalues_3[(int)((long)(i_15))]) / (double)(total_1)))));
            i_15 = (long)((long)(i_15) + 1L);
        }
        return new PCAResult(transformed_1, ratios_1);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            result_2 = apply_pca(((double[][])(data)), 2L);
            System.out.println("Transformed Data (first 5 rows):");
            while ((long)(idx) < 5L) {
                System.out.println(java.util.Arrays.toString(result_2.transformed[(int)((long)(idx))]));
                idx = (long)((long)(idx) + 1L);
            }
            System.out.println("Explained Variance Ratio:");
            System.out.println(java.util.Arrays.toString(result_2.variance_ratio));
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

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
