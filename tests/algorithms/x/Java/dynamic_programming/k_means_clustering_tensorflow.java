public class Main {
    static class KMeansResult {
        double[][] centroids;
        int[] assignments;
        KMeansResult(double[][] centroids, int[] assignments) {
            this.centroids = centroids;
            this.assignments = assignments;
        }
        KMeansResult() {}
        @Override public String toString() {
            return String.format("{'centroids': %s, 'assignments': %s}", String.valueOf(centroids), String.valueOf(assignments));
        }
    }


    static double distance_sq(double[] a, double[] b) {
        double sum = 0.0;
        int i = 0;
        while (i < a.length) {
            double diff = a[i] - b[i];
            sum = sum + diff * diff;
            i = i + 1;
        }
        return sum;
    }

    static double[] mean(double[][] vectors) {
        int dim = vectors[0].length;
        double[] res = ((double[])(new double[]{}));
        int i_1 = 0;
        while (i_1 < dim) {
            double total = 0.0;
            int j = 0;
            while (j < vectors.length) {
                total = total + vectors[j][i_1];
                j = j + 1;
            }
            res = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(res), java.util.stream.DoubleStream.of(total / vectors.length)).toArray()));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static KMeansResult k_means(double[][] vectors, int k, int iterations) {
        double[][] centroids = ((double[][])(new double[][]{}));
        int i_2 = 0;
        while (i_2 < k) {
            centroids = ((double[][])(appendObj((double[][])centroids, vectors[i_2])));
            i_2 = i_2 + 1;
        }
        int[] assignments = ((int[])(new int[]{}));
        int n = vectors.length;
        i_2 = 0;
        while (i_2 < n) {
            assignments = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(assignments), java.util.stream.IntStream.of(0)).toArray()));
            i_2 = i_2 + 1;
        }
        int it = 0;
        while (it < iterations) {
            int v = 0;
            while (v < n) {
                int best = 0;
                double bestDist = distance_sq(((double[])(vectors[v])), ((double[])(centroids[0])));
                int c = 1;
                while (c < k) {
                    double d = distance_sq(((double[])(vectors[v])), ((double[])(centroids[c])));
                    if (d < bestDist) {
                        bestDist = d;
                        best = c;
                    }
                    c = c + 1;
                }
assignments[v] = best;
                v = v + 1;
            }
            int cIdx = 0;
            while (cIdx < k) {
                double[][] cluster = ((double[][])(new double[][]{}));
                int v2 = 0;
                while (v2 < n) {
                    if (assignments[v2] == cIdx) {
                        cluster = ((double[][])(appendObj((double[][])cluster, vectors[v2])));
                    }
                    v2 = v2 + 1;
                }
                if (cluster.length > 0) {
centroids[cIdx] = ((double[])(mean(((double[][])(cluster)))));
                }
                cIdx = cIdx + 1;
            }
            it = it + 1;
        }
        return new KMeansResult(centroids, assignments);
    }

    static void main() {
        double[][] vectors = ((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{1.5, 1.8}, new double[]{5.0, 8.0}, new double[]{8.0, 8.0}, new double[]{1.0, 0.6}, new double[]{9.0, 11.0}}));
        KMeansResult result = k_means(((double[][])(vectors)), 2, 5);
        System.out.println(_p(result.centroids));
        System.out.println(_p(result.assignments));
    }
    public static void main(String[] args) {
        main();
    }

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        return String.valueOf(v);
    }
}
