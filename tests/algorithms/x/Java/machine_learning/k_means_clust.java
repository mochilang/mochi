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

    static double[][] data;
    static long k;
    static double[][] initial_centroids;
    static KMeansResult result;

    static double distance_sq(double[] a, double[] b) {
        double sum = 0.0;
        for (int i = 0; i < a.length; i++) {
            double diff_1 = a[(int)(i)] - b[(int)(i)];
            sum = sum + diff_1 * diff_1;
        }
        return sum;
    }

    static long[] assign_clusters(double[][] data, double[][] centroids) {
        long[] assignments = ((long[])(new long[]{}));
        for (int i = 0; i < data.length; i++) {
            long best_idx_1 = 0;
            double best_1 = distance_sq(((double[])(data[(int)(i)])), ((double[])(centroids[(int)(0)])));
            for (int j = 1; j < centroids.length; j++) {
                double dist_1 = distance_sq(((double[])(data[(int)(i)])), ((double[])(centroids[(int)(j)])));
                if (dist_1 < best_1) {
                    best_1 = dist_1;
                    best_idx_1 = j;
                }
            }
            assignments = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(assignments), java.util.stream.LongStream.of(best_idx_1)).toArray()));
        }
        return assignments;
    }

    static double[][] revise_centroids(double[][] data, long k, long[] assignment) {
        long dim = data[(int)(0)].length;
        double[][] sums_1 = ((double[][])(new double[][]{}));
        long[] counts_1 = ((long[])(new long[]{}));
        for (int i = 0; i < k; i++) {
            double[] row_1 = ((double[])(new double[]{}));
            for (int j = 0; j < dim; j++) {
                row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
            }
            sums_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(sums_1), java.util.stream.Stream.of(row_1)).toArray(double[][]::new)));
            counts_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(counts_1), java.util.stream.LongStream.of(0)).toArray()));
        }
        for (int i = 0; i < data.length; i++) {
            long c_1 = assignment[(int)(i)];
counts_1[(int)(c_1)] = counts_1[(int)(c_1)] + 1;
            for (int j = 0; j < dim; j++) {
sums_1[(int)(c_1)][(int)(j)] = sums_1[(int)(c_1)][(int)(j)] + data[(int)(i)][(int)(j)];
            }
        }
        double[][] centroids_1 = ((double[][])(new double[][]{}));
        for (int i = 0; i < k; i++) {
            double[] row_3 = ((double[])(new double[]{}));
            if (counts_1[(int)(i)] > 0) {
                for (int j = 0; j < dim; j++) {
                    row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(sums_1[(int)(i)][(int)(j)] / (((double)(counts_1[(int)(i)]))))).toArray()));
                }
            } else {
                for (int j = 0; j < dim; j++) {
                    row_3 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_3), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
            }
            centroids_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(centroids_1), java.util.stream.Stream.of(row_3)).toArray(double[][]::new)));
        }
        return centroids_1;
    }

    static double compute_heterogeneity(double[][] data, double[][] centroids, long[] assignment) {
        double total = 0.0;
        for (int i = 0; i < data.length; i++) {
            long c_3 = assignment[(int)(i)];
            total = total + distance_sq(((double[])(data[(int)(i)])), ((double[])(centroids[(int)(c_3)])));
        }
        return total;
    }

    static boolean lists_equal(long[] a, long[] b) {
        if (a.length != b.length) {
            return false;
        }
        for (int i = 0; i < a.length; i++) {
            if (a[(int)(i)] != b[(int)(i)]) {
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
        long iter_1 = 0;
        while (iter_1 < max_iter) {
            assignment_1 = ((long[])(assign_clusters(((double[][])(data)), ((double[][])(centroids_2)))));
            centroids_2 = ((double[][])(revise_centroids(((double[][])(data)), k, ((long[])(assignment_1)))));
            double h_1 = compute_heterogeneity(((double[][])(data)), ((double[][])(centroids_2)), ((long[])(assignment_1)));
            heterogeneity_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(heterogeneity_1), java.util.stream.DoubleStream.of(h_1)).toArray()));
            if (iter_1 > 0 && ((Boolean)(lists_equal(((long[])(prev_1)), ((long[])(assignment_1)))))) {
                break;
            }
            prev_1 = ((long[])(assignment_1));
            iter_1 = iter_1 + 1;
        }
        return new KMeansResult(centroids_2, assignment_1, heterogeneity_1);
    }
    public static void main(String[] args) {
        data = ((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{1.5, 1.8}, new double[]{5.0, 8.0}, new double[]{8.0, 8.0}, new double[]{1.0, 0.6}, new double[]{9.0, 11.0}}));
        k = 3;
        initial_centroids = ((double[][])(new double[][]{data[(int)(0)], data[(int)(2)], data[(int)(5)]}));
        result = kmeans(((double[][])(data)), k, ((double[][])(initial_centroids)), 10);
        System.out.println(_p(result.centroids));
        System.out.println(_p(result.assignments));
        System.out.println(_p(result.heterogeneity));
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
