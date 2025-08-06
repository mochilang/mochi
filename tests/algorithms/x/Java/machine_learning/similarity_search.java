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
    static int k = 0;

    static double sqrt(double x) {
        if (x <= 0.0) {
            return 0.0;
        }
        double guess = x;
        int i = 0;
        while (i < 10) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double euclidean(double[] a, double[] b) {
        double sum = 0.0;
        int i_1 = 0;
        while (i_1 < a.length) {
            double diff = a[i_1] - b[i_1];
            sum = sum + diff * diff;
            i_1 = i_1 + 1;
        }
        double res = sqrt(sum);
        return res;
    }

    static Neighbor[] similarity_search(double[][] dataset, double[][] value_array) {
        int dim = dataset[0].length;
        if (dim != value_array[0].length) {
            return new Neighbor[]{};
        }
        Neighbor[] result = ((Neighbor[])(new Neighbor[]{}));
        int i_2 = 0;
        while (i_2 < value_array.length) {
            double[] value = ((double[])(value_array[i_2]));
            double dist = euclidean(((double[])(value)), ((double[])(dataset[0])));
            double[] vec = ((double[])(dataset[0]));
            int j = 1;
            while (j < dataset.length) {
                double d = euclidean(((double[])(value)), ((double[])(dataset[j])));
                if (d < dist) {
                    dist = d;
                    vec = ((double[])(dataset[j]));
                }
                j = j + 1;
            }
            Neighbor nb = new Neighbor(vec, dist);
            result = ((Neighbor[])(java.util.stream.Stream.concat(java.util.Arrays.stream(result), java.util.stream.Stream.of(nb)).toArray(Neighbor[]::new)));
            i_2 = i_2 + 1;
        }
        return result;
    }

    static double cosine_similarity(double[] a, double[] b) {
        double dot = 0.0;
        double norm_a = 0.0;
        double norm_b = 0.0;
        int i_3 = 0;
        while (i_3 < a.length) {
            dot = dot + a[i_3] * b[i_3];
            norm_a = norm_a + a[i_3] * a[i_3];
            norm_b = norm_b + b[i_3] * b[i_3];
            i_3 = i_3 + 1;
        }
        if (norm_a == 0.0 || norm_b == 0.0) {
            return 0.0;
        }
        return dot / (sqrt(norm_a) * sqrt(norm_b));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            dataset = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{1.0, 1.0, 1.0}, new double[]{2.0, 2.0, 2.0}}));
            value_array = ((double[][])(new double[][]{new double[]{0.0, 0.0, 0.0}, new double[]{0.0, 0.0, 1.0}}));
            neighbors = ((Neighbor[])(similarity_search(((double[][])(dataset)), ((double[][])(value_array)))));
            k = 0;
            while (k < neighbors.length) {
                Neighbor n = neighbors[k];
                System.out.println("[" + _p(n.vector) + ", " + _p(n.distance) + "]");
                k = k + 1;
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
        return String.valueOf(v);
    }
}
