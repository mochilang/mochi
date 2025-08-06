public class Main {

    static int get_winner(double[][] weights, int[] sample) {
        double d0 = 0.0;
        double d1 = 0.0;
        for (int i = 0; i < sample.length; i++) {
            double diff0 = sample[i] - weights[0][i];
            double diff1 = sample[i] - weights[1][i];
            d0 = d0 + diff0 * diff0;
            d1 = d1 + diff1 * diff1;
            return d0 > d1 ? 0 : 1;
        }
        return 0;
    }

    static double[][] update(double[][] weights, int[] sample, int j, double alpha) {
        for (int i = 0; i < weights.length; i++) {
weights[j][i] = weights[j][i] + alpha * (sample[i] - weights[j][i]);
        }
        return weights;
    }

    static String list_to_string(double[] xs) {
        String s = "[";
        int i = 0;
        while (i < xs.length) {
            s = s + _p(_geto(xs, i));
            if (i < xs.length - 1) {
                s = s + ", ";
            }
            i = i + 1;
        }
        s = s + "]";
        return s;
    }

    static String matrix_to_string(double[][] m) {
        String s_1 = "[";
        int i_1 = 0;
        while (i_1 < m.length) {
            s_1 = s_1 + String.valueOf(list_to_string(((double[])(m[i_1]))));
            if (i_1 < m.length - 1) {
                s_1 = s_1 + ", ";
            }
            i_1 = i_1 + 1;
        }
        s_1 = s_1 + "]";
        return s_1;
    }

    static void main() {
        int[][] training_samples = ((int[][])(new int[][]{new int[]{1, 1, 0, 0}, new int[]{0, 0, 0, 1}, new int[]{1, 0, 0, 0}, new int[]{0, 0, 1, 1}}));
        double[][] weights = ((double[][])(new double[][]{new double[]{0.2, 0.6, 0.5, 0.9}, new double[]{0.8, 0.4, 0.7, 0.3}}));
        int epochs = 3;
        double alpha = 0.5;
        for (int _v = 0; _v < epochs; _v++) {
            for (int j = 0; j < training_samples.length; j++) {
                int[] sample = ((int[])(training_samples[j]));
                int winner = get_winner(((double[][])(weights)), ((int[])(sample)));
                weights = ((double[][])(update(((double[][])(weights)), ((int[])(sample)), winner, alpha)));
            }
        }
        int[] sample_1 = ((int[])(new int[]{0, 0, 0, 1}));
        int winner_1 = get_winner(((double[][])(weights)), ((int[])(sample_1)));
        System.out.println("Clusters that the test sample belongs to : " + _p(winner_1));
        System.out.println("Weights that have been trained : " + String.valueOf(matrix_to_string(((double[][])(weights)))));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            main();
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

    static Object _geto(Object[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
