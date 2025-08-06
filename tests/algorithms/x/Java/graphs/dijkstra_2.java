public class Main {
    static double INF;

    static void print_dist(double[] dist) {
        System.out.println("Vertex Distance");
        int i = 0;
        while (i < dist.length) {
            if (dist[i] >= INF) {
                System.out.println(String.valueOf(i) + " " + "\tINF");
            } else {
                System.out.println(String.valueOf(i) + " " + "\t" + " " + String.valueOf(((int)(dist[i]))));
            }
            i = i + 1;
        }
    }

    static int min_dist(double[] mdist, boolean[] vset) {
        double min_val = INF;
        int min_ind = -1;
        int i_1 = 0;
        while (i_1 < mdist.length) {
            if (!(Boolean)(vset[i_1]) && mdist[i_1] < min_val) {
                min_val = mdist[i_1];
                min_ind = i_1;
            }
            i_1 = i_1 + 1;
        }
        return min_ind;
    }

    static double[] dijkstra(double[][] graph, int src) {
        int v = graph.length;
        double[] mdist = ((double[])(new double[]{}));
        boolean[] vset = ((boolean[])(new boolean[]{}));
        int i_2 = 0;
        while (i_2 < v) {
            mdist = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(mdist), java.util.stream.DoubleStream.of(INF)).toArray()));
            vset = ((boolean[])(appendBool(vset, false)));
            i_2 = i_2 + 1;
        }
mdist[src] = 0.0;
        int count = 0;
        while (count < v - 1) {
            int u = min_dist(((double[])(mdist)), ((boolean[])(vset)));
vset[u] = true;
            int i_3 = 0;
            while (i_3 < v) {
                double alt = mdist[u] + graph[u][i_3];
                if (!(Boolean)(vset[i_3]) && graph[u][i_3] < INF && alt < mdist[i_3]) {
mdist[i_3] = alt;
                }
                i_3 = i_3 + 1;
            }
            count = count + 1;
        }
        return mdist;
    }

    static void main() {
        double[][] graph = ((double[][])(new double[][]{new double[]{0.0, 10.0, INF, INF, 5.0}, new double[]{INF, 0.0, 1.0, INF, 2.0}, new double[]{INF, INF, 0.0, 4.0, INF}, new double[]{INF, INF, 6.0, 0.0, INF}, new double[]{INF, 3.0, 9.0, 2.0, 0.0}}));
        double[] dist = ((double[])(dijkstra(((double[][])(graph)), 0)));
        print_dist(((double[])(dist)));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            INF = 1000000000.0;
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
