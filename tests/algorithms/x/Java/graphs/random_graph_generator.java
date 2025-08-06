public class Main {
    static int seed = 0;

    static int rand() {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random() {
        return (1.0 * rand()) / 2147483648.0;
    }

    static java.util.Map<Integer,int[]> complete_graph(int vertices_number) {
        java.util.Map<Integer,int[]> graph = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
        int i = 0;
        while (i < vertices_number) {
            int[] neighbors = ((int[])(new int[]{}));
            int j = 0;
            while (j < vertices_number) {
                if (j != i) {
                    neighbors = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(neighbors), java.util.stream.IntStream.of(j)).toArray()));
                }
                j = j + 1;
            }
graph.put(i, ((int[])(neighbors)));
            i = i + 1;
        }
        return graph;
    }

    static java.util.Map<Integer,int[]> random_graph(int vertices_number, double probability, boolean directed) {
        java.util.Map<Integer,int[]> graph_1 = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>()));
        int i_1 = 0;
        while (i_1 < vertices_number) {
graph_1.put(i_1, ((int[])(new int[]{})));
            i_1 = i_1 + 1;
        }
        if (probability >= 1.0) {
            return complete_graph(vertices_number);
        }
        if (probability <= 0.0) {
            return graph_1;
        }
        i_1 = 0;
        while (i_1 < vertices_number) {
            int j_1 = i_1 + 1;
            while (j_1 < vertices_number) {
                if (random() < probability) {
graph_1.put(i_1, ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(((int[])(graph_1).get(i_1))), java.util.stream.IntStream.of(j_1)).toArray())));
                    if (!(Boolean)directed) {
graph_1.put(j_1, ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(((int[])(graph_1).get(j_1))), java.util.stream.IntStream.of(i_1)).toArray())));
                    }
                }
                j_1 = j_1 + 1;
            }
            i_1 = i_1 + 1;
        }
        return graph_1;
    }

    static void main() {
        seed = 1;
        java.util.Map<Integer,int[]> g1 = random_graph(4, 0.5, false);
        System.out.println(g1);
        seed = 1;
        java.util.Map<Integer,int[]> g2 = random_graph(4, 0.5, true);
        System.out.println(g2);
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            seed = 1;
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
}
