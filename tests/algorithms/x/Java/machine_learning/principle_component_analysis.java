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
    static PCAResult result_2;
    static long idx = 0;

    static double sqrt(double x) {
        double guess = x > 1.0 ? x / 2.0 : 1.0;
        long i_1 = 0;
        while (i_1 < 20) {
            guess = 0.5 * (guess + x / guess);
            i_1 = i_1 + 1;
        }
        return guess;
    }

    static double mean(double[] xs) {
        double sum = 0.0;
        long i_3 = 0;
        while (i_3 < xs.length) {
            sum = sum + xs[(int)(i_3)];
            i_3 = i_3 + 1;
        }
        return sum / xs.length;
    }

    static double[][] standardize(double[][] data) {
        long n_samples = data.length;
        long n_features_1 = data[(int)(0)].length;
        double[] means_1 = ((double[])(new double[]{}));
        double[] stds_1 = ((double[])(new double[]{}));
        long j_1 = 0;
        while (j_1 < n_features_1) {
            double[] column_1 = ((double[])(new double[]{}));
            long i_5 = 0;
            while (i_5 < n_samples) {
                column_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(column_1), java.util.stream.DoubleStream.of(data[(int)(i_5)][(int)(j_1)])).toArray()));
                i_5 = i_5 + 1;
            }
            double m_1 = mean(((double[])(column_1)));
            means_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(means_1), java.util.stream.DoubleStream.of(m_1)).toArray()));
            double variance_1 = 0.0;
            long k_1 = 0;
            while (k_1 < n_samples) {
                double diff_1 = column_1[(int)(k_1)] - m_1;
                variance_1 = variance_1 + diff_1 * diff_1;
                k_1 = k_1 + 1;
            }
            stds_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(stds_1), java.util.stream.DoubleStream.of(sqrt(variance_1 / (n_samples - 1)))).toArray()));
            j_1 = j_1 + 1;
        }
        double[][] standardized_1 = ((double[][])(new double[][]{}));
        long r_1 = 0;
        while (r_1 < n_samples) {
            double[] row_1 = ((double[])(new double[]{}));
            long c_1 = 0;
            while (c_1 < n_features_1) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of((data[(int)(r_1)][(int)(c_1)] - means_1[(int)(c_1)]) / stds_1[(int)(c_1)])).toArray()));
                c_1 = c_1 + 1;
            }
            standardized_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(standardized_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            r_1 = r_1 + 1;
        }
        return standardized_1;
    }

    static double[][] covariance_matrix(double[][] data) {
        long n_samples_1 = data.length;
        long n_features_3 = data[(int)(0)].length;
        double[][] cov_1 = ((double[][])(new double[][]{}));
        long i_7 = 0;
        while (i_7 < n_features_3) {
            double[] row_3 = ((double[])(new double[]{}));
            long j_3 = 0;
            while (j_3 < n_features_3) {
                double sum_2 = 0.0;
                long k_3 = 0;
                while (k_3 < n_samples_1) {
                    sum_2 = sum_2 + data[(int)(k_3)][(int)(i_7)] * data[(int)(k_3)][(int)(j_3)];
                    k_3 = k_3 + 1;
                }
                row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(sum_2 / (n_samples_1 - 1))).toArray()));
                j_3 = j_3 + 1;
            }
            cov_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(cov_1), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
            i_7 = i_7 + 1;
        }
        return cov_1;
    }

    static double[] normalize(double[] vec) {
        double sum_3 = 0.0;
        long i_9 = 0;
        while (i_9 < vec.length) {
            sum_3 = sum_3 + vec[(int)(i_9)] * vec[(int)(i_9)];
            i_9 = i_9 + 1;
        }
        double n_1 = sqrt(sum_3);
        double[] res_1 = ((double[])(new double[]{}));
        long j_5 = 0;
        while (j_5 < vec.length) {
            res_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res_1), java.util.stream.DoubleStream.of(vec[(int)(j_5)] / n_1)).toArray()));
            j_5 = j_5 + 1;
        }
        return res_1;
    }

    static Eigen eigen_decomposition_2x2(double[][] matrix) {
        double a = matrix[(int)(0)][(int)(0)];
        double b_1 = matrix[(int)(0)][(int)(1)];
        double c_3 = matrix[(int)(1)][(int)(1)];
        double diff_3 = a - c_3;
        double discriminant_1 = sqrt(diff_3 * diff_3 + 4.0 * b_1 * b_1);
        double lambda1_1 = (a + c_3 + discriminant_1) / 2.0;
        double lambda2_1 = (a + c_3 - discriminant_1) / 2.0;
        double[] v1_1 = new double[0];
        double[] v2_1 = new double[0];
        if (b_1 != 0.0) {
            v1_1 = ((double[])(normalize(((double[])(new Object[]{lambda1_1 - c_3, b_1})))));
            v2_1 = ((double[])(normalize(((double[])(new Object[]{lambda2_1 - c_3, b_1})))));
        } else {
            v1_1 = ((double[])(new double[]{1.0, 0.0}));
            v2_1 = ((double[])(new double[]{0.0, 1.0}));
        }
        double[] eigenvalues_1 = ((double[])(new double[]{lambda1_1, lambda2_1}));
        double[][] eigenvectors_1 = ((double[][])(new double[][]{v1_1, v2_1}));
        if (eigenvalues_1[(int)(0)] < eigenvalues_1[(int)(1)]) {
            double tmp_val_1 = eigenvalues_1[(int)(0)];
eigenvalues_1[(int)(0)] = eigenvalues_1[(int)(1)];
eigenvalues_1[(int)(1)] = tmp_val_1;
            double[] tmp_vec_1 = ((double[])(eigenvectors_1[(int)(0)]));
eigenvectors_1[(int)(0)] = ((double[])(eigenvectors_1[(int)(1)]));
eigenvectors_1[(int)(1)] = ((double[])(tmp_vec_1));
        }
        return new Eigen(eigenvalues_1, eigenvectors_1);
    }

    static double[][] transpose(double[][] matrix) {
        long rows = matrix.length;
        long cols_1 = matrix[(int)(0)].length;
        double[][] trans_1 = ((double[][])(new double[][]{}));
        long i_11 = 0;
        while (i_11 < cols_1) {
            double[] row_5 = ((double[])(new double[]{}));
            long j_7 = 0;
            while (j_7 < rows) {
                row_5 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_5), java.util.stream.DoubleStream.of(matrix[(int)(j_7)][(int)(i_11)])).toArray()));
                j_7 = j_7 + 1;
            }
            trans_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(trans_1), java.util.stream.Stream.of(row_5)).toArray(double[][]::new)));
            i_11 = i_11 + 1;
        }
        return trans_1;
    }

    static double[][] matrix_multiply(double[][] a, double[][] b) {
        long rows_a = a.length;
        long cols_a_1 = a[(int)(0)].length;
        long rows_b_1 = b.length;
        long cols_b_1 = b[(int)(0)].length;
        if (cols_a_1 != rows_b_1) {
            throw new RuntimeException(String.valueOf("Incompatible matrices"));
        }
        double[][] result_1 = ((double[][])(new double[][]{}));
        long i_13 = 0;
        while (i_13 < rows_a) {
            double[] row_7 = ((double[])(new double[]{}));
            long j_9 = 0;
            while (j_9 < cols_b_1) {
                double sum_5 = 0.0;
                long k_5 = 0;
                while (k_5 < cols_a_1) {
                    sum_5 = sum_5 + a[(int)(i_13)][(int)(k_5)] * b[(int)(k_5)][(int)(j_9)];
                    k_5 = k_5 + 1;
                }
                row_7 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_7), java.util.stream.DoubleStream.of(sum_5)).toArray()));
                j_9 = j_9 + 1;
            }
            result_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(row_7)).toArray(double[][]::new)));
            i_13 = i_13 + 1;
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
        double total_1 = eigenvalues_3[(int)(0)] + eigenvalues_3[(int)(1)];
        double[] ratios_1 = ((double[])(new double[]{}));
        long i_15 = 0;
        while (i_15 < n_components) {
            ratios_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(ratios_1), java.util.stream.DoubleStream.of(eigenvalues_3[(int)(i_15)] / total_1)).toArray()));
            i_15 = i_15 + 1;
        }
        return new PCAResult(transformed_1, ratios_1);
    }
    public static void main(String[] args) {
        data = ((double[][])(new double[][]{new double[]{2.5, 2.4}, new double[]{0.5, 0.7}, new double[]{2.2, 2.9}, new double[]{1.9, 2.2}, new double[]{3.1, 3.0}, new double[]{2.3, 2.7}, new double[]{2.0, 1.6}, new double[]{1.0, 1.1}, new double[]{1.5, 1.6}, new double[]{1.1, 0.9}}));
        result_2 = apply_pca(((double[][])(data)), 2);
        System.out.println("Transformed Data (first 5 rows):");
        idx = 0;
        while (idx < 5) {
            System.out.println(java.util.Arrays.toString(result_2.transformed[(int)(idx)]));
            idx = idx + 1;
        }
        System.out.println("Explained Variance Ratio:");
        System.out.println(java.util.Arrays.toString(result_2.variance_ratio));
    }
}
