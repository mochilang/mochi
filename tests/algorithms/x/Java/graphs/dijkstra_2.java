public class Main {
    static double INF = (double)(1000000000.0);

    static void print_dist(double[] dist) {
        System.out.println("Vertex Distance");
        long i_1 = 0L;
        while ((long)(i_1) < (long)(dist.length)) {
            if ((double)(dist[(int)((long)(i_1))]) >= (double)(INF)) {
                System.out.println(String.valueOf(i_1) + " " + "\tINF");
            } else {
                System.out.println(String.valueOf(i_1) + " " + "\t" + " " + String.valueOf(((long)(dist[(int)((long)(i_1))]))));
            }
            i_1 = (long)((long)(i_1) + 1L);
        }
    }

    static long min_dist(double[] mdist, boolean[] vset) {
        double min_val = (double)(INF);
        long min_ind_1 = (long)(-1);
        long i_3 = 0L;
        while ((long)(i_3) < (long)(mdist.length)) {
            if (!(Boolean)(vset[(int)((long)(i_3))]) && (double)(mdist[(int)((long)(i_3))]) < (double)(min_val)) {
                min_val = (double)(mdist[(int)((long)(i_3))]);
                min_ind_1 = (long)(i_3);
            }
            i_3 = (long)((long)(i_3) + 1L);
        }
        return min_ind_1;
    }

    static double[] dijkstra(double[][] graph, long src) {
        long v = (long)(graph.length);
        double[] mdist_1 = ((double[])(new double[]{}));
        boolean[] vset_1 = ((boolean[])(new boolean[]{}));
        long i_5 = 0L;
        while ((long)(i_5) < (long)(v)) {
            mdist_1 = ((double[])(appendDouble(mdist_1, (double)(INF))));
            vset_1 = ((boolean[])(appendBool(vset_1, false)));
            i_5 = (long)((long)(i_5) + 1L);
        }
mdist_1[(int)((long)(src))] = (double)(0.0);
        long count_1 = 0L;
        while ((long)(count_1) < (long)((long)(v) - 1L)) {
            long u_1 = (long)(min_dist(((double[])(mdist_1)), ((boolean[])(vset_1))));
vset_1[(int)((long)(u_1))] = true;
            long i_7 = 0L;
            while ((long)(i_7) < (long)(v)) {
                double alt_1 = (double)((double)(mdist_1[(int)((long)(u_1))]) + (double)(graph[(int)((long)(u_1))][(int)((long)(i_7))]));
                if (!(Boolean)(vset_1[(int)((long)(i_7))]) && (double)(graph[(int)((long)(u_1))][(int)((long)(i_7))]) < (double)(INF) && (double)(alt_1) < (double)(mdist_1[(int)((long)(i_7))])) {
mdist_1[(int)((long)(i_7))] = (double)(alt_1);
                }
                i_7 = (long)((long)(i_7) + 1L);
            }
            count_1 = (long)((long)(count_1) + 1L);
        }
        return mdist_1;
    }

    static void main() {
        double[][] graph = ((double[][])(new double[][]{new double[]{0.0, 10.0, INF, INF, 5.0}, new double[]{INF, 0.0, 1.0, INF, 2.0}, new double[]{INF, INF, 0.0, 4.0, INF}, new double[]{INF, INF, 6.0, 0.0, INF}, new double[]{INF, 3.0, 9.0, 2.0, 0.0}}));
        double[] dist_1 = ((double[])(dijkstra(((double[][])(graph)), 0L)));
        print_dist(((double[])(dist_1)));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }

    static double[] appendDouble(double[] arr, double v) {
        double[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
