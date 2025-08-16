public class Main {
    static class KMeansResult {
        double[][] centroids;
        long[] assignments;
        double[] heterogeneity;
        KMeansResult(double[][] centroids, long[] assignments, double[] heterogeneity) {
            this.centroids = centroids;
            this.assignments = assignments;
            this.heterogeneity = heterogeneity;
        }
        KMeansResult() {}
        @Override public String toString() {
            return String.format("{'centroids': %s, 'assignments': %s, 'heterogeneity': %s}", String.valueOf(centroids), String.valueOf(assignments), String.valueOf(heterogeneity));
        }
    }

    static double[][] data = ((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{1.5, 1.8}, new double[]{5.0, 8.0}, new double[]{8.0, 8.0}, new double[]{1.0, 0.6}, new double[]{9.0, 11.0}}));
    static long k = 3L;
    static double[][] initial_centroids;
    static KMeansResult result;

    static double distance_sq(double[] a, double[] b) {
        double sum = (double)(0.0);
        for (int i = 0; i < a.length; i++) {
            double diff_1 = (double)((double)(a[(int)((long)(i))]) - (double)(b[(int)((long)(i))]));
            sum = (double)((double)(sum) + (double)((double)(diff_1) * (double)(diff_1)));
        }
        return sum;
    }

    static long[] assign_clusters(double[][] data, double[][] centroids) {
        long[] assignments = ((long[])(new long[]{}));
        for (int i = 0; i < data.length; i++) {
            long best_idx_1 = 0L;
            double best_1 = (double)(distance_sq(((double[])(data[(int)((long)(i))])), ((double[])(centroids[(int)(0L)]))));
            for (int j = 1; j < centroids.length; j++) {
                double dist_1 = (double)(distance_sq(((double[])(data[(int)((long)(i))])), ((double[])(centroids[(int)((long)(j))]))));
                if ((double)(dist_1) < (double)(best_1)) {
                    best_1 = (double)(dist_1);
                    best_idx_1 = (long)(j);
                }
            }
            assignments = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(assignments), java.util.stream.LongStream.of((long)(best_idx_1))).toArray()));
        }
        return assignments;
    }

    static double[][] revise_centroids(double[][] data, long k, long[] assignment) {
        long dim = (long)(data[(int)(0L)].length);
        double[][] sums_1 = ((double[][])(new double[][]{}));
        long[] counts_1 = ((long[])(new long[]{}));
        for (int i = 0; i < k; i++) {
            double[] row_1 = ((double[])(new double[]{}));
            for (int j = 0; j < dim; j++) {
                row_1 = ((double[])(appendDouble(row_1, (double)(0.0))));
            }
            sums_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(sums_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            counts_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(counts_1), java.util.stream.LongStream.of(0L)).toArray()));
        }
        for (int i = 0; i < data.length; i++) {
            long c_1 = (long)(assignment[(int)((long)(i))]);
counts_1[(int)((long)(c_1))] = (long)((long)(counts_1[(int)((long)(c_1))]) + 1L);
            for (int j = 0; j < dim; j++) {
sums_1[(int)((long)(c_1))][(int)((long)(j))] = (double)((double)(sums_1[(int)((long)(c_1))][(int)((long)(j))]) + (double)(data[(int)((long)(i))][(int)((long)(j))]));
            }
        }
        double[][] centroids_1 = ((double[][])(new double[][]{}));
        for (int i = 0; i < k; i++) {
            double[] row_3 = ((double[])(new double[]{}));
            if ((long)(counts_1[(int)((long)(i))]) > 0L) {
                for (int j = 0; j < dim; j++) {
                    row_3 = ((double[])(appendDouble(row_3, (double)((double)(sums_1[(int)((long)(i))][(int)((long)(j))]) / (double)((((double)(counts_1[(int)((long)(i))]))))))));
                }
            } else {
                for (int j = 0; j < dim; j++) {
                    row_3 = ((double[])(appendDouble(row_3, (double)(0.0))));
                }
            }
            centroids_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(centroids_1), java.util.stream.Stream.of(new double[][]{row_3})).toArray(double[][]::new)));
        }
        return centroids_1;
    }

    static double compute_heterogeneity(double[][] data, double[][] centroids, long[] assignment) {
        double total = (double)(0.0);
        for (int i = 0; i < data.length; i++) {
            long c_3 = (long)(assignment[(int)((long)(i))]);
            total = (double)((double)(total) + (double)(distance_sq(((double[])(data[(int)((long)(i))])), ((double[])(centroids[(int)((long)(c_3))])))));
        }
        return total;
    }

    static boolean lists_equal(long[] a, long[] b) {
        if ((long)(a.length) != (long)(b.length)) {
            return false;
        }
        for (int i = 0; i < a.length; i++) {
            if ((long)(a[(int)((long)(i))]) != (long)(b[(int)((long)(i))])) {
                return false;
            }
        }
        return true;
    }

    static KMeansResult kmeans(double[][] data, long k, double[][] initial_centroids, long max_iter) {
        double[][] centroids_2 = ((double[][])(initial_centroids));
        long[] assignment_1 = ((long[])(new long[]{}));
        long[] prev_1 = ((long[])(new long[]{}));
        double[] heterogeneity_1 = ((double[])(new double[]{}));
        long iter_1 = 0L;
        while ((long)(iter_1) < (long)(max_iter)) {
            assignment_1 = ((long[])(assign_clusters(((double[][])(data)), ((double[][])(centroids_2)))));
            centroids_2 = ((double[][])(revise_centroids(((double[][])(data)), (long)(k), ((long[])(assignment_1)))));
            double h_1 = (double)(compute_heterogeneity(((double[][])(data)), ((double[][])(centroids_2)), ((long[])(assignment_1))));
            heterogeneity_1 = ((double[])(appendDouble(heterogeneity_1, (double)(h_1))));
            if ((long)(iter_1) > 0L && lists_equal(((long[])(prev_1)), ((long[])(assignment_1)))) {
                break;
            }
            prev_1 = ((long[])(assignment_1));
            iter_1 = (long)((long)(iter_1) + 1L);
        }
        return new KMeansResult(centroids_2, assignment_1, heterogeneity_1);
    }
    public static void main(String[] args) {
        initial_centroids = ((double[][])(new double[][]{data[(int)(0L)], data[(int)(2L)], data[(int)(5L)]}));
        result = kmeans(((double[][])(data)), (long)(k), ((double[][])(initial_centroids)), 10L);
        System.out.println(_p(result.centroids));
        System.out.println(_p(result.assignments));
        System.out.println(_p(result.heterogeneity));
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
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
