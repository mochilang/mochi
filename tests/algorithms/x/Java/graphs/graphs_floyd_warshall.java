public class Main {
    static double INF = (double)(1000000000.0);
    static double[][] graph = ((double[][])(new double[][]{new double[]{0.0, 5.0, INF, 10.0}, new double[]{INF, 0.0, 3.0, INF}, new double[]{INF, INF, 0.0, 1.0}, new double[]{INF, INF, INF, 0.0}}));
    static double[][] result;

    static double[][] floyd_warshall(double[][] graph) {
        long v = (long)(graph.length);
        double[][] dist_1 = ((double[][])(new double[][]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(v)) {
            double[] row_1 = ((double[])(new double[]{}));
            long j_1 = 0L;
            while ((long)(j_1) < (long)(v)) {
                row_1 = ((double[])(appendDouble(row_1, (double)(graph[(int)((long)(i_1))][(int)((long)(j_1))]))));
                j_1 = (long)((long)(j_1) + 1L);
            }
            dist_1 = ((double[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(dist_1), java.util.stream.Stream.of(new double[][]{row_1})).toArray(double[][]::new)));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long k_1 = 0L;
        while ((long)(k_1) < (long)(v)) {
            long i_3 = 0L;
            while ((long)(i_3) < (long)(v)) {
                long j_3 = 0L;
                while ((long)(j_3) < (long)(v)) {
                    if ((double)(dist_1[(int)((long)(i_3))][(int)((long)(k_1))]) < (double)(INF) && (double)(dist_1[(int)((long)(k_1))][(int)((long)(j_3))]) < (double)(INF) && (double)((double)(dist_1[(int)((long)(i_3))][(int)((long)(k_1))]) + (double)(dist_1[(int)((long)(k_1))][(int)((long)(j_3))])) < (double)(dist_1[(int)((long)(i_3))][(int)((long)(j_3))])) {
dist_1[(int)((long)(i_3))][(int)((long)(j_3))] = (double)((double)(dist_1[(int)((long)(i_3))][(int)((long)(k_1))]) + (double)(dist_1[(int)((long)(k_1))][(int)((long)(j_3))]));
                    }
                    j_3 = (long)((long)(j_3) + 1L);
                }
                i_3 = (long)((long)(i_3) + 1L);
            }
            k_1 = (long)((long)(k_1) + 1L);
        }
        return dist_1;
    }

    static void print_dist(double[][] dist) {
        System.out.println("\nThe shortest path matrix using Floyd Warshall algorithm\n");
        long i_5 = 0L;
        while ((long)(i_5) < (long)(dist.length)) {
            long j_5 = 0L;
            String line_1 = "";
            while ((long)(j_5) < (long)(dist[(int)((long)(i_5))].length)) {
                if ((double)(dist[(int)((long)(i_5))][(int)((long)(j_5))]) >= (double)((double)(INF) / (double)(2.0))) {
                    line_1 = line_1 + "INF\t";
                } else {
                    line_1 = line_1 + _p(((Number)(_getd(dist[(int)((long)(i_5))], ((Number)(j_5)).intValue()))).intValue()) + "\t";
                }
                j_5 = (long)((long)(j_5) + 1L);
            }
            System.out.println(line_1);
            i_5 = (long)((long)(i_5) + 1L);
        }
    }
    public static void main(String[] args) {
        result = ((double[][])(floyd_warshall(((double[][])(graph)))));
        print_dist(((double[][])(result)));
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

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
