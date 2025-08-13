public class Main {
    static class Edge {
        long src;
        long dst;
        long weight;
        Edge(long src, long dst, long weight) {
            this.src = src;
            this.dst = dst;
            this.weight = weight;
        }
        Edge() {}
        @Override public String toString() {
            return String.format("{'src': %s, 'dst': %s, 'weight': %s}", String.valueOf(src), String.valueOf(dst), String.valueOf(weight));
        }
    }

    static double INF = (double)(1000000000.0);
    static Edge[] edges;
    static double[] distances;

    static String list_to_string(double[] arr) {
        String s = "[";
        long i_1 = 0L;
        while ((long)(i_1) < (long)(arr.length)) {
            s = s + _p(_getd(arr, ((Number)(i_1)).intValue()));
            if ((long)(i_1) < (long)((long)(arr.length) - (long)(1))) {
                s = s + ", ";
            }
            i_1 = (long)((long)(i_1) + (long)(1));
        }
        return s + "]";
    }

    static boolean check_negative_cycle(Edge[] graph, double[] distance, long edge_count) {
        long j = 0L;
        while ((long)(j) < (long)(edge_count)) {
            Edge e_1 = graph[(int)((long)(j))];
            long u_1 = (long)(e_1.src);
            long v_1 = (long)(e_1.dst);
            double w_1 = (double)(((Number)(e_1.weight)).doubleValue());
            if ((double)(distance[(int)((long)(u_1))]) < (double)(INF) && (double)((double)(distance[(int)((long)(u_1))]) + (double)(w_1)) < (double)(distance[(int)((long)(v_1))])) {
                return true;
            }
            j = (long)((long)(j) + (long)(1));
        }
        return false;
    }

    static double[] bellman_ford(Edge[] graph, long vertex_count, long edge_count, long src) {
        double[] distance = ((double[])(new double[]{}));
        long i_3 = 0L;
        while ((long)(i_3) < (long)(vertex_count)) {
            distance = ((double[])(appendDouble(distance, (double)(INF))));
            i_3 = (long)((long)(i_3) + (long)(1));
        }
distance[(int)((long)(src))] = (double)(0.0);
        long k_1 = 0L;
        while ((long)(k_1) < (long)((long)(vertex_count) - (long)(1))) {
            long j_2 = 0L;
            while ((long)(j_2) < (long)(edge_count)) {
                Edge e_3 = graph[(int)((long)(j_2))];
                long u_3 = (long)(e_3.src);
                long v_3 = (long)(e_3.dst);
                double w_3 = (double)(((Number)(e_3.weight)).doubleValue());
                if ((double)(distance[(int)((long)(u_3))]) < (double)(INF) && (double)((double)(distance[(int)((long)(u_3))]) + (double)(w_3)) < (double)(distance[(int)((long)(v_3))])) {
distance[(int)((long)(v_3))] = (double)((double)(distance[(int)((long)(u_3))]) + (double)(w_3));
                }
                j_2 = (long)((long)(j_2) + (long)(1));
            }
            k_1 = (long)((long)(k_1) + (long)(1));
        }
        if (check_negative_cycle(((Edge[])(graph)), ((double[])(distance)), (long)(edge_count))) {
            throw new RuntimeException(String.valueOf("Negative cycle found"));
        }
        return distance;
    }
    public static void main(String[] args) {
        edges = ((Edge[])(new Edge[]{new Edge(2, 1, -10), new Edge(3, 2, 3), new Edge(0, 3, 5), new Edge(0, 1, 4)}));
        distances = ((double[])(bellman_ford(((Edge[])(edges)), 4L, (long)(edges.length), 0L)));
        System.out.println(list_to_string(((double[])(distances))));
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
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }

    static Double _getd(double[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
