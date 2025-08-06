public class Main {
    static class KMeansResult {
        double[][] centroids;
        int[] assignments;
        double[] heterogeneity;
        KMeansResult(double[][] centroids, int[] assignments, double[] heterogeneity) {
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
    static int k;
    static double[][] initial_centroids;
    static KMeansResult result;

    static double distance_sq(double[] a, double[] b) {
        double sum = 0.0;
        for (int i = 0; i < a.length; i++) {
            double diff = a[i] - b[i];
            sum = sum + diff * diff;
        }
        return sum;
    }

    static int[] assign_clusters(double[][] data, double[][] centroids) {
        int[] assignments = ((int[])(new int[]{}));
        for (int i = 0; i < data.length; i++) {
            int best_idx = 0;
            double best = distance_sq(((double[])(data[i])), ((double[])(centroids[0])));
            for (int j = 1; j < centroids.length; j++) {
                double dist = distance_sq(((double[])(data[i])), ((double[])(centroids[j])));
                if (dist < best) {
                    best = dist;
                    best_idx = j;
                }
            }
            assignments = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(assignments), java.util.stream.IntStream.of(best_idx)).toArray()));
        }
        return assignments;
    }

    static double[][] revise_centroids(double[][] data, int k, int[] assignment) {
        int dim = data[0].length;
        double[][] sums = ((double[][])(new double[][]{}));
        int[] counts = ((int[])(new int[]{}));
        for (int i = 0; i < k; i++) {
            double[] row = ((double[])(new double[]{}));
            for (int j = 0; j < dim; j++) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(0.0)).toArray()));
            }
            sums = ((double[][])(appendObj(sums, row)));
            counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(0)).toArray()));
        }
        for (int i = 0; i < data.length; i++) {
            int c = assignment[i];
counts[c] = counts[c] + 1;
            for (int j = 0; j < dim; j++) {
sums[c][j] = sums[c][j] + data[i][j];
            }
        }
        double[][] centroids = ((double[][])(new double[][]{}));
        for (int i = 0; i < k; i++) {
            double[] row_1 = ((double[])(new double[]{}));
            if (counts[i] > 0) {
                for (int j = 0; j < dim; j++) {
                    row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(sums[i][j] / (((double)(counts[i]))))).toArray()));
                }
            } else {
                for (int j = 0; j < dim; j++) {
                    row_1 = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row_1), java.util.stream.DoubleStream.of(0.0)).toArray()));
                }
            }
            centroids = ((double[][])(appendObj(centroids, row_1)));
        }
        return centroids;
    }

    static double compute_heterogeneity(double[][] data, double[][] centroids, int[] assignment) {
        double total = 0.0;
        for (int i = 0; i < data.length; i++) {
            int c_1 = assignment[i];
            total = total + distance_sq(((double[])(data[i])), ((double[])(centroids[c_1])));
        }
        return total;
    }

    static boolean lists_equal(int[] a, int[] b) {
        if (a.length != b.length) {
            return false;
        }
        for (int i = 0; i < a.length; i++) {
            if (a[i] != b[i]) {
                return false;
            }
        }
        return true;
    }

    static KMeansResult kmeans(double[][] data, int k, double[][] initial_centroids, int max_iter) {
        double[][] centroids_1 = ((double[][])(initial_centroids));
        int[] assignment = ((int[])(new int[]{}));
        int[] prev = ((int[])(new int[]{}));
        double[] heterogeneity = ((double[])(new double[]{}));
        int iter = 0;
        while (iter < max_iter) {
            assignment = ((int[])(assign_clusters(((double[][])(data)), ((double[][])(centroids_1)))));
            centroids_1 = ((double[][])(revise_centroids(((double[][])(data)), k, ((int[])(assignment)))));
            double h = compute_heterogeneity(((double[][])(data)), ((double[][])(centroids_1)), ((int[])(assignment)));
            heterogeneity = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(heterogeneity), java.util.stream.DoubleStream.of(h)).toArray()));
            if (iter > 0 && ((Boolean)(lists_equal(((int[])(prev)), ((int[])(assignment)))))) {
                break;
            }
            prev = ((int[])(assignment));
            iter = iter + 1;
        }
        return new KMeansResult(centroids_1, assignment, heterogeneity);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            data = ((double[][])(new double[][]{new double[]{1.0, 2.0}, new double[]{1.5, 1.8}, new double[]{5.0, 8.0}, new double[]{8.0, 8.0}, new double[]{1.0, 0.6}, new double[]{9.0, 11.0}}));
            k = 3;
            initial_centroids = ((double[][])(new double[][]{data[0], data[2], data[5]}));
            result = kmeans(((double[][])(data)), k, ((double[][])(initial_centroids)), 10);
            System.out.println(_p(result.centroids));
            System.out.println(_p(result.assignments));
            System.out.println(_p(result.heterogeneity));
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
