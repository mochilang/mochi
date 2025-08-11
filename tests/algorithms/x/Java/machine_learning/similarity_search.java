public class Main {
    static class Neighbor {
        double[] vector;
        double distance;
        Neighbor(double[] vector, double distance) {
            this.vector = vector;
            this.distance = distance;
        }
        Neighbor() {}
        @Override public String toString() {
            return String.format("{'vector': %s, 'distance': %s}", String.valueOf(vector), String.valueOf(distance));
        }
    }

    static double[][] dataset;
    static double[][] value_array;
    static Neighbor[] neighbors;
    static long k = 0;

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess_1 = x;
        long i_1 = 0;
        while (i_1 < 10) {
            guess_1 = (guess_1 + x / guess_1) / 2.0;
            i_1 = i_1 + 1;
        }
        return guess_1;
    }

    static double euclidean(double[] a, double[] b) {
        double sum = 0.0;
        long i_3 = 0;
        while (i_3 < a.length) {
            double diff_1 = a[(int)(i_3)] - b[(int)(i_3)];
            sum = sum + diff_1 * diff_1;
            i_3 = i_3 + 1;
        }
        double res_1 = sqrt(sum);
        return res_1;
    }

    static Neighbor[] similarity_search(double[][] dataset, double[][] value_array) {
        long dim = dataset[(int)(0)].length;
        if (dim != value_array[(int)(0)].length) {
            return new Neighbor[]{};
        }
        Neighbor[] result_1 = ((Neighbor[])(new Neighbor[]{}));
        long i_5 = 0;
        while (i_5 < value_array.length) {
            double[] value_1 = ((double[])(value_array[(int)(i_5)]));
            double dist_1 = euclidean(((double[])(value_1)), ((double[])(dataset[(int)(0)])));
            double[] vec_1 = ((double[])(dataset[(int)(0)]));
            long j_1 = 1;
            while (j_1 < dataset.length) {
                double d_1 = euclidean(((double[])(value_1)), ((double[])(dataset[(int)(j_1)])));
                if (d_1 < dist_1) {
                    dist_1 = d_1;
                    vec_1 = ((double[])(dataset[(int)(j_1)]));
                }
                j_1 = j_1 + 1;
            }
            Neighbor nb_1 = new Neighbor(vec_1, dist_1);
            result_1 = ((Neighbor[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(nb_1)).toArray(Neighbor[]::new)));
            i_5 = i_5 + 1;
        }
        return result_1;
    }

    static double cosine_similarity(double[] a, double[] b) {
        double dot = 0.0;
        double norm_a_1 = 0.0;
        double norm_b_1 = 0.0;
        long i_7 = 0;
        while (i_7 < a.length) {
            dot = dot + a[(int)(i_7)] * b[(int)(i_7)];
            norm_a_1 = norm_a_1 + a[(int)(i_7)] * a[(int)(i_7)];
            norm_b_1 = norm_b_1 + b[(int)(i_7)] * b[(int)(i_7)];
            i_7 = i_7 + 1;
        }
        if (norm_a_1 == 0.0 || norm_b_1 == 0.0) {
            return 0.0;
        }
        return dot / (sqrt(norm_a_1) * sqrt(norm_b_1));
    }
    public static void main(String[] args) {
        dataset = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{1.0, 1.0, 1.0}, new double[]{2.0, 2.0, 2.0}}));
        value_array = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0}}));
        neighbors = ((Neighbor[])(similarity_search(((double[][])(dataset)), ((double[][])(value_array)))));
        k = 0;
        while (k < neighbors.length) {
            Neighbor n = neighbors[(int)(k)];
            System.out.println("[" + _p(n.vector) + ", " + _p(n.distance) + "]");
            k = k + 1;
        }
        System.out.println(_p(cosine_similarity(((double[])(new double[]{1.0, 2.0})), ((double[])(new double[]{6.0, 32.0})))));
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
