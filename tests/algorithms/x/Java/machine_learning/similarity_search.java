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

    static double[][] dataset = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{1.0, 1.0, 1.0}, new double[]{2.0, 2.0, 2.0}}));
    static double[][] value_array = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0}}));
    static Neighbor[] neighbors;
    static long k = 0L;

    static double sqrt(double x) {
        if ((double)(x) <= (double)(0.0)) {
            return 0.0;
        }
        double guess_1 = (double)(x);
        long i_1 = 0L;
        while ((long)(i_1) < 10L) {
            guess_1 = (double)((double)(((double)(guess_1) + (double)((double)(x) / (double)(guess_1)))) / (double)(2.0));
            i_1 = (long)((long)(i_1) + 1L);
        }
        return guess_1;
    }

    static double euclidean(double[] a, double[] b) {
        double sum = (double)(0.0);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(a.length)) {
            double diff_1 = (double)((double)(a[(int)((long)(i_3))]) - (double)(b[(int)((long)(i_3))]));
            sum = (double)((double)(sum) + (double)((double)(diff_1) * (double)(diff_1)));
            i_3 = (long)((long)(i_3) + 1L);
        }
        double res_1 = (double)(sqrt((double)(sum)));
        return res_1;
    }

    static Neighbor[] similarity_search(double[][] dataset, double[][] value_array) {
        long dim = (long)(dataset[(int)(0L)].length);
        if ((long)(dim) != (long)(value_array[(int)(0L)].length)) {
            return new Neighbor[]{};
        }
        Neighbor[] result_1 = ((Neighbor[])(new Neighbor[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(value_array.length)) {
            double[] value_1 = ((double[])(value_array[(int)((long)(i_5))]));
            double dist_1 = (double)(euclidean(((double[])(value_1)), ((double[])(dataset[(int)(0L)]))));
            double[] vec_1 = ((double[])(dataset[(int)(0L)]));
            long j_1 = 1L;
            while ((long)(j_1) < (long)(dataset.length)) {
                double d_1 = (double)(euclidean(((double[])(value_1)), ((double[])(dataset[(int)((long)(j_1))]))));
                if ((double)(d_1) < (double)(dist_1)) {
                    dist_1 = (double)(d_1);
                    vec_1 = ((double[])(dataset[(int)((long)(j_1))]));
                }
                j_1 = (long)((long)(j_1) + 1L);
            }
            Neighbor nb_1 = new Neighbor(vec_1, dist_1);
            result_1 = ((Neighbor[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result_1), java.util.stream.Stream.of(nb_1)).toArray(Neighbor[]::new)));
            i_5 = (long)((long)(i_5) + 1L);
        }
        return result_1;
    }

    static double cosine_similarity(double[] a, double[] b) {
        double dot = (double)(0.0);
        double norm_a_1 = (double)(0.0);
        double norm_b_1 = (double)(0.0);
        long i_7 = 0L;
        while ((long)(i_7) < (long)(a.length)) {
            dot = (double)((double)(dot) + (double)((double)(a[(int)((long)(i_7))]) * (double)(b[(int)((long)(i_7))])));
            norm_a_1 = (double)((double)(norm_a_1) + (double)((double)(a[(int)((long)(i_7))]) * (double)(a[(int)((long)(i_7))])));
            norm_b_1 = (double)((double)(norm_b_1) + (double)((double)(b[(int)((long)(i_7))]) * (double)(b[(int)((long)(i_7))])));
            i_7 = (long)((long)(i_7) + 1L);
        }
        if ((double)(norm_a_1) == (double)(0.0) || (double)(norm_b_1) == (double)(0.0)) {
            return 0.0;
        }
        return (double)(dot) / (double)(((double)(sqrt((double)(norm_a_1))) * (double)(sqrt((double)(norm_b_1)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            neighbors = ((Neighbor[])(similarity_search(((double[][])(dataset)), ((double[][])(value_array)))));
            while ((long)(k) < (long)(neighbors.length)) {
                Neighbor n = neighbors[(int)((long)(k))];
                System.out.println("[" + _p(n.vector) + ", " + _p(n.distance) + "]");
                k = (long)((long)(k) + 1L);
            }
            System.out.println(_p(cosine_similarity(((double[])(new double[]{1.0, 2.0})), ((double[])(new double[]{6.0, 32.0})))));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
