public class Main {
    static double W1;
    static double W2;
    static int n;
    static int n_heuristic;
    static class Pos {
        int x;
        int y;
        Pos(int x, int y) {
            this.x = x;
            this.y = y;
        }
        Pos() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s}", String.valueOf(x), String.valueOf(y));
        }
    }

    static class PQNode {
        Pos pos;
        double pri;
        PQNode(Pos pos, double pri) {
            this.pos = pos;
            this.pri = pri;
        }
        PQNode() {}
        @Override public String toString() {
            return String.format("{'pos': %s, 'pri': %s}", String.valueOf(pos), String.valueOf(pri));
        }
    }

    static class PQPopResult {
        PQNode[] pq;
        PQNode node;
        PQPopResult(PQNode[] pq, PQNode node) {
            this.pq = pq;
            this.node = node;
        }
        PQPopResult() {}
        @Override public String toString() {
            return String.format("{'pq': %s, 'node': %s}", String.valueOf(pq), String.valueOf(node));
        }
    }

    static double INF;
    static int t = 0;
    static Pos[] blocks;
    static Pos start;
    static Pos goal;

    static boolean pos_equal(Pos a, Pos b) {
        return a.x == b.x && a.y == b.y;
    }

    static String pos_key(Pos p) {
        return _p(p.x) + "," + _p(p.y);
    }

    static double sqrtApprox(double x) {
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

    static double consistent_heuristic(Pos p, Pos goal) {
        double dx = ((Number)((p.x - goal.x))).doubleValue();
        double dy = ((Number)((p.y - goal.y))).doubleValue();
        return sqrtApprox(dx * dx + dy * dy);
    }

    static int iabs(int x) {
        if (x < 0) {
            return -x;
        }
        return x;
    }

    static double heuristic_1(Pos p, Pos goal) {
        return ((Number)((iabs(p.x - goal.x) + iabs(p.y - goal.y)))).doubleValue();
    }

    static double heuristic_2(Pos p, Pos goal) {
        double h = consistent_heuristic(p, goal);
        return h / (((Number)(t)).doubleValue());
    }

    static double heuristic(int i, Pos p, Pos goal) {
        if (i == 0) {
            return consistent_heuristic(p, goal);
        }
        if (i == 1) {
            return heuristic_1(p, goal);
        }
        return heuristic_2(p, goal);
    }

    static double key_fn(Pos start, int i, Pos goal, java.util.Map<String,Double> g_func) {
        double g = (double)(((double)(g_func).getOrDefault(pos_key(start), 0.0)));
        return g + W1 * heuristic(i, start, goal);
    }

    static boolean valid(Pos p) {
        if (p.x < 0 || p.x > n - 1) {
            return false;
        }
        if (p.y < 0 || p.y > n - 1) {
            return false;
        }
        return true;
    }

    static boolean in_blocks(Pos p) {
        int i_1 = 0;
        while (i_1 < blocks.length) {
            if (((Boolean)(pos_equal(blocks[i_1], p)))) {
                return true;
            }
            i_1 = i_1 + 1;
        }
        return false;
    }

    static PQNode[] pq_put(PQNode[] pq, Pos node, double pri) {
        boolean updated = false;
        int i_2 = 0;
        while (i_2 < pq.length) {
            if (((Boolean)(pos_equal(pq[i_2].pos, node)))) {
                if (pri < pq[i_2].pri) {
pq[i_2] = new PQNode(node, pri);
                }
                updated = true;
            }
            i_2 = i_2 + 1;
        }
        if (!updated) {
            pq = ((PQNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(pq), java.util.stream.Stream.of(new PQNode(node, pri))).toArray(PQNode[]::new)));
        }
        return pq;
    }

    static double pq_minkey(PQNode[] pq) {
        if (pq.length == 0) {
            return INF;
        }
        PQNode first = pq[0];
        double m = first.pri;
        int i_3 = 1;
        while (i_3 < pq.length) {
            PQNode item = pq[i_3];
            if (item.pri < m) {
                m = item.pri;
            }
            i_3 = i_3 + 1;
        }
        return m;
    }

    static PQPopResult pq_pop_min(PQNode[] pq) {
        PQNode best = pq[0];
        int idx = 0;
        int i_4 = 1;
        while (i_4 < pq.length) {
            if (pq[i_4].pri < best.pri) {
                best = pq[i_4];
                idx = i_4;
            }
            i_4 = i_4 + 1;
        }
        PQNode[] new_pq = ((PQNode[])(new PQNode[]{}));
        i_4 = 0;
        while (i_4 < pq.length) {
            if (i_4 != idx) {
                new_pq = ((PQNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_pq), java.util.stream.Stream.of(pq[i_4])).toArray(PQNode[]::new)));
            }
            i_4 = i_4 + 1;
        }
        return new PQPopResult(new_pq, best);
    }

    static PQNode[] pq_remove(PQNode[] pq, Pos node) {
        PQNode[] new_pq_1 = ((PQNode[])(new PQNode[]{}));
        int i_5 = 0;
        while (i_5 < pq.length) {
            if (!(Boolean)pos_equal(pq[i_5].pos, node)) {
                new_pq_1 = ((PQNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_pq_1), java.util.stream.Stream.of(pq[i_5])).toArray(PQNode[]::new)));
            }
            i_5 = i_5 + 1;
        }
        return new_pq_1;
    }

    static Pos[] reconstruct(java.util.Map<String,Pos> back_pointer, Pos goal, Pos start) {
        Pos[] path = ((Pos[])(new Pos[]{}));
        Pos current = goal;
        String key = String.valueOf(pos_key(current));
        path = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(current)).toArray(Pos[]::new)));
        while (!(Boolean)(pos_equal(current, start))) {
            current = (Pos)(((Pos)(back_pointer).get(key)));
            key = String.valueOf(pos_key(current));
            path = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(current)).toArray(Pos[]::new)));
        }
        Pos[] rev = ((Pos[])(new Pos[]{}));
        int i_6 = path.length - 1;
        while (i_6 >= 0) {
            rev = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rev), java.util.stream.Stream.of(path[i_6])).toArray(Pos[]::new)));
            i_6 = i_6 - 1;
        }
        return rev;
    }

    static Pos[] neighbours(Pos p) {
        Pos left = new Pos(p.x - 1, p.y);
        Pos right = new Pos(p.x + 1, p.y);
        Pos up = new Pos(p.x, p.y + 1);
        Pos down = new Pos(p.x, p.y - 1);
        return new Pos[]{left, right, up, down};
    }

    static void multi_a_star(Pos start, Pos goal, int n_heuristic) {
        java.util.Map<String,Double> g_function = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        java.util.Map<String,Pos> back_pointer = ((java.util.Map<String,Pos>)(new java.util.LinkedHashMap<String, Pos>()));
        java.util.Map<String,Boolean> visited = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        PQNode[][] open_list = ((PQNode[][])(new PQNode[][]{}));
g_function.put(pos_key(start), 0.0);
g_function.put(pos_key(goal), INF);
back_pointer.put(pos_key(start), new Pos(-1, -1));
back_pointer.put(pos_key(goal), new Pos(-1, -1));
visited.put(pos_key(start), true);
        int i_7 = 0;
        while (i_7 < n_heuristic) {
            open_list = ((PQNode[][])(appendObj(open_list, new PQNode[]{})));
            double pri = key_fn(start, i_7, goal, g_function);
open_list[i_7] = ((PQNode[])(pq_put(((PQNode[])(open_list[i_7])), start, pri)));
            i_7 = i_7 + 1;
        }
        while (pq_minkey(((PQNode[])(open_list[0]))) < INF) {
            int chosen = 0;
            i_7 = 1;
            while (i_7 < n_heuristic) {
                if (pq_minkey(((PQNode[])(open_list[i_7]))) <= W2 * pq_minkey(((PQNode[])(open_list[0])))) {
                    chosen = i_7;
                    break;
                }
                i_7 = i_7 + 1;
            }
            if (chosen != 0) {
                t = t + 1;
            }
            PQPopResult pair = pq_pop_min(((PQNode[])(open_list[chosen])));
open_list[chosen] = ((PQNode[])(pair.pq));
            PQNode current_1 = pair.node;
            i_7 = 0;
            while (i_7 < n_heuristic) {
                if (i_7 != chosen) {
open_list[i_7] = ((PQNode[])(pq_remove(((PQNode[])(open_list[i_7])), current_1.pos)));
                }
                i_7 = i_7 + 1;
            }
            String ckey = String.valueOf(pos_key(current_1.pos));
            if (((Boolean)(visited.containsKey(ckey)))) {
                continue;
            }
visited.put(ckey, true);
            if (((Boolean)(pos_equal(current_1.pos, goal)))) {
                Pos[] path_1 = ((Pos[])(reconstruct(back_pointer, goal, start)));
                int j = 0;
                while (j < path_1.length) {
                    Pos p = path_1[j];
                    System.out.println("(" + _p(p.x) + "," + _p(p.y) + ")");
                    j = j + 1;
                }
                return;
            }
            Pos[] neighs = ((Pos[])(neighbours(current_1.pos)));
            int k = 0;
            while (k < neighs.length) {
                Pos nb = neighs[k];
                if (((Boolean)(valid(nb))) && (in_blocks(nb) == false)) {
                    String nkey = String.valueOf(pos_key(nb));
                    double tentative = (double)(((double)(g_function).getOrDefault(ckey, 0.0))) + 1.0;
                    if (!(Boolean)(g_function.containsKey(nkey)) || tentative < (double)(((double)(g_function).getOrDefault(nkey, 0.0)))) {
g_function.put(nkey, tentative);
back_pointer.put(nkey, current_1.pos);
                        i_7 = 0;
                        while (i_7 < n_heuristic) {
                            double pri2 = tentative + W1 * heuristic(i_7, nb, goal);
open_list[i_7] = ((PQNode[])(pq_put(((PQNode[])(open_list[i_7])), nb, pri2)));
                            i_7 = i_7 + 1;
                        }
                    }
                }
                k = k + 1;
            }
        }
        System.out.println("No path found to goal");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            W1 = 1.0;
            W2 = 1.0;
            n = 20;
            n_heuristic = 3;
            INF = 1000000000.0;
            t = 1;
            blocks = ((Pos[])(new Pos[]{new Pos(0, 1), new Pos(1, 1), new Pos(2, 1), new Pos(3, 1), new Pos(4, 1), new Pos(5, 1), new Pos(6, 1), new Pos(7, 1), new Pos(8, 1), new Pos(9, 1), new Pos(10, 1), new Pos(11, 1), new Pos(12, 1), new Pos(13, 1), new Pos(14, 1), new Pos(15, 1), new Pos(16, 1), new Pos(17, 1), new Pos(18, 1), new Pos(19, 1)}));
            start = new Pos(0, 0);
            goal = new Pos(n - 1, n - 1);
            multi_a_star(start, goal, n_heuristic);
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

    static <T> T[] appendObj(T[] arr, T v) {
        T[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
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
        return String.valueOf(v);
    }
}
