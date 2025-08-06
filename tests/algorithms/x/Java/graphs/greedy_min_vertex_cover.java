public class Main {
    static java.util.Map<Integer,int[]> graph = null;

    static int[] remove_value(int[] lst, int val) {
        int[] res = ((int[])(new int[]{}));
        int i = 0;
        while (i < lst.length) {
            if (lst[i] != val) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(lst[i])).toArray()));
            }
            i = i + 1;
        }
        return res;
    }

    static int[] greedy_min_vertex_cover(java.util.Map<Integer,int[]> graph) {
        java.util.Map<Integer,int[]> g = graph;
        int[] cover = ((int[])(new int[]{}));
        while (true) {
            int max_v = 0;
            int max_deg = 0;
            for (int v : g.keySet()) {
                int key = ((Number)(v)).intValue();
                int deg = ((int[])(g).get(key)).length;
                if (deg > max_deg) {
                    max_deg = deg;
                    max_v = key;
                }
            }
            if (max_deg == 0) {
                break;
            }
            cover = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(cover), java.util.stream.IntStream.of(max_v)).toArray()));
            int[] neighbors = (int[])(((int[])(g).get(max_v)));
            int i_1 = 0;
            while (i_1 < neighbors.length) {
                int n = neighbors[i_1];
g.put(n, ((int[])(remove_value((int[])(((int[])(g).get(n))), max_v))));
                i_1 = i_1 + 1;
            }
g.put(max_v, ((int[])(new int[]{})));
        }
        return cover;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 3}))), java.util.Map.entry(1, ((int[])(new int[]{0, 3}))), java.util.Map.entry(2, ((int[])(new int[]{0, 3, 4}))), java.util.Map.entry(3, ((int[])(new int[]{0, 1, 2}))), java.util.Map.entry(4, ((int[])(new int[]{2, 3})))))));
            System.out.println(greedy_min_vertex_cover(graph));
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
