public class Main {
    static double W1 = (double)(1.0);
    static double W2 = (double)(1.0);
    static long n = 20L;
    static long n_heuristic = 3L;
    static class Pos {
        long x;
        long y;
        Pos(long x, long y) {
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

    static double INF = (double)(1000000000.0);
    static long t = 1L;
    static Pos[] blocks;
    static Pos start;
    static Pos goal;

    static boolean pos_equal(Pos a, Pos b) {
        return (long)(a.x) == (long)(b.x) && (long)(a.y) == (long)(b.y);
    }

    static String pos_key(Pos p) {
        return _p(p.x) + "," + _p(p.y);
    }

    static double sqrtApprox(double x) {
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

    static double consistent_heuristic(Pos p, Pos goal) {
        double dx = (double)(((Number)(((long)(p.x) - (long)(goal.x)))).doubleValue());
        double dy_1 = (double)(((Number)(((long)(p.y) - (long)(goal.y)))).doubleValue());
        return sqrtApprox((double)((double)((double)(dx) * (double)(dx)) + (double)((double)(dy_1) * (double)(dy_1))));
    }

    static long iabs(long x) {
        if ((long)(x) < 0L) {
            return -x;
        }
        return x;
    }

    static double heuristic_1(Pos p, Pos goal) {
        return ((Number)(((long)(iabs((long)((long)(p.x) - (long)(goal.x)))) + (long)(iabs((long)((long)(p.y) - (long)(goal.y))))))).doubleValue();
    }

    static double heuristic_2(Pos p, Pos goal) {
        double h = (double)(consistent_heuristic(p, goal));
        return (double)(h) / (double)((((Number)(t)).doubleValue()));
    }

    static double heuristic(long i, Pos p, Pos goal) {
        if ((long)(i) == 0L) {
            return consistent_heuristic(p, goal);
        }
        if ((long)(i) == 1L) {
            return heuristic_1(p, goal);
        }
        return heuristic_2(p, goal);
    }

    static double key_fn(Pos start, long i, Pos goal, java.util.Map<String,Double> g_func) {
        double g = (double)(((double)(g_func).getOrDefault(pos_key(start), 0.0)));
        return (double)(g) + (double)((double)(W1) * (double)(heuristic((long)(i), start, goal)));
    }

    static boolean valid(Pos p) {
        if ((long)(p.x) < 0L || (long)(p.x) > (long)((long)(n) - 1L)) {
            return false;
        }
        if ((long)(p.y) < 0L || (long)(p.y) > (long)((long)(n) - 1L)) {
            return false;
        }
        return true;
    }

    static boolean in_blocks(Pos p) {
        long i_2 = 0L;
        while ((long)(i_2) < (long)(blocks.length)) {
            if (pos_equal(blocks[(int)((long)(i_2))], p)) {
                return true;
            }
            i_2 = (long)((long)(i_2) + 1L);
        }
        return false;
    }

    static PQNode[] pq_put(PQNode[] pq, Pos node, double pri) {
        boolean updated = false;
        long i_4 = 0L;
        while ((long)(i_4) < (long)(pq.length)) {
            if (pos_equal(pq[(int)((long)(i_4))].pos, node)) {
                if ((double)(pri) < (double)(pq[(int)((long)(i_4))].pri)) {
pq[(int)((long)(i_4))] = new PQNode(node, pri);
                }
                updated = true;
            }
            i_4 = (long)((long)(i_4) + 1L);
        }
        if (!updated) {
            pq = ((PQNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(pq), java.util.stream.Stream.of(new PQNode(node, pri))).toArray(PQNode[]::new)));
        }
        return pq;
    }

    static double pq_minkey(PQNode[] pq) {
        if ((long)(pq.length) == 0L) {
            return INF;
        }
        PQNode first_1 = pq[(int)((long)(0))];
        double m_1 = (double)(first_1.pri);
        long i_6 = 1L;
        while ((long)(i_6) < (long)(pq.length)) {
            PQNode item_1 = pq[(int)((long)(i_6))];
            if ((double)(item_1.pri) < (double)(m_1)) {
                m_1 = (double)(item_1.pri);
            }
            i_6 = (long)((long)(i_6) + 1L);
        }
        return m_1;
    }

    static PQPopResult pq_pop_min(PQNode[] pq) {
        PQNode best = pq[(int)((long)(0))];
        long idx_1 = 0L;
        long i_8 = 1L;
        while ((long)(i_8) < (long)(pq.length)) {
            if ((double)(pq[(int)((long)(i_8))].pri) < (double)(best.pri)) {
                best = pq[(int)((long)(i_8))];
                idx_1 = (long)(i_8);
            }
            i_8 = (long)((long)(i_8) + 1L);
        }
        PQNode[] new_pq_1 = ((PQNode[])(new PQNode[]{}));
        i_8 = 0L;
        while ((long)(i_8) < (long)(pq.length)) {
            if ((long)(i_8) != (long)(idx_1)) {
                new_pq_1 = ((PQNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_pq_1), java.util.stream.Stream.of(pq[(int)((long)(i_8))])).toArray(PQNode[]::new)));
            }
            i_8 = (long)((long)(i_8) + 1L);
        }
        return new PQPopResult(new_pq_1, best);
    }

    static PQNode[] pq_remove(PQNode[] pq, Pos node) {
        PQNode[] new_pq_2 = ((PQNode[])(new PQNode[]{}));
        long i_10 = 0L;
        while ((long)(i_10) < (long)(pq.length)) {
            if (!(Boolean)pos_equal(pq[(int)((long)(i_10))].pos, node)) {
                new_pq_2 = ((PQNode[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_pq_2), java.util.stream.Stream.of(pq[(int)((long)(i_10))])).toArray(PQNode[]::new)));
            }
            i_10 = (long)((long)(i_10) + 1L);
        }
        return new_pq_2;
    }

    static Pos[] reconstruct(java.util.Map<String,Pos> back_pointer, Pos goal, Pos start) {
        Pos[] path = ((Pos[])(new Pos[]{}));
        Pos current_1 = goal;
        String key_1 = String.valueOf(pos_key(current_1));
        path = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(current_1)).toArray(Pos[]::new)));
        while (!(Boolean)(pos_equal(current_1, start))) {
            current_1 = (Pos)(((Pos)(back_pointer).get(key_1)));
            key_1 = String.valueOf(pos_key(current_1));
            path = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(current_1)).toArray(Pos[]::new)));
        }
        Pos[] rev_1 = ((Pos[])(new Pos[]{}));
        long i_12 = (long)((long)(path.length) - 1L);
        while ((long)(i_12) >= 0L) {
            rev_1 = ((Pos[])(java.util.stream.Stream.concat(java.util.Arrays.stream(rev_1), java.util.stream.Stream.of(path[(int)((long)(i_12))])).toArray(Pos[]::new)));
            i_12 = (long)((long)(i_12) - 1L);
        }
        return rev_1;
    }

    static Pos[] neighbours(Pos p) {
        Pos left = new Pos((long)(p.x) - 1L, p.y);
        Pos right_1 = new Pos((long)(p.x) + 1L, p.y);
        Pos up_1 = new Pos(p.x, (long)(p.y) + 1L);
        Pos down_1 = new Pos(p.x, (long)(p.y) - 1L);
        return new Pos[]{left, right_1, up_1, down_1};
    }

    static void multi_a_star(Pos start, Pos goal, long n_heuristic) {
        java.util.Map<String,Double> g_function = ((java.util.Map<String,Double>)(new java.util.LinkedHashMap<String, Double>()));
        java.util.Map<String,Pos> back_pointer_1 = ((java.util.Map<String,Pos>)(new java.util.LinkedHashMap<String, Pos>()));
        java.util.Map<String,Boolean> visited_1 = ((java.util.Map<String,Boolean>)(new java.util.LinkedHashMap<String, Boolean>()));
        PQNode[][] open_list_1 = ((PQNode[][])(new PQNode[][]{}));
g_function.put(pos_key(start), (double)(0.0));
g_function.put(pos_key(goal), (double)(INF));
back_pointer_1.put(pos_key(start), new Pos(-1, -1));
back_pointer_1.put(pos_key(goal), new Pos(-1, -1));
visited_1.put(pos_key(start), true);
        long i_14 = 0L;
        while ((long)(i_14) < (long)(n_heuristic)) {
            open_list_1 = ((PQNode[][])(java.util.stream.Stream.concat(java.util.Arrays.stream(open_list_1), java.util.stream.Stream.of(new PQNode[][]{new PQNode[]{}})).toArray(PQNode[][]::new)));
            double pri_1 = (double)(key_fn(start, (long)(i_14), goal, g_function));
open_list_1[(int)((long)(i_14))] = ((PQNode[])(pq_put(((PQNode[])(open_list_1[(int)((long)(i_14))])), start, (double)(pri_1))));
            i_14 = (long)((long)(i_14) + 1L);
        }
        while ((double)(pq_minkey(((PQNode[])(open_list_1[(int)((long)(0))])))) < (double)(INF)) {
            long chosen_1 = 0L;
            i_14 = 1L;
            while ((long)(i_14) < (long)(n_heuristic)) {
                if ((double)(pq_minkey(((PQNode[])(open_list_1[(int)((long)(i_14))])))) <= (double)((double)(W2) * (double)(pq_minkey(((PQNode[])(open_list_1[(int)((long)(0))])))))) {
                    chosen_1 = (long)(i_14);
                    break;
                }
                i_14 = (long)((long)(i_14) + 1L);
            }
            if ((long)(chosen_1) != 0L) {
                t = (long)((long)(t) + 1L);
            }
            PQPopResult pair_1 = pq_pop_min(((PQNode[])(open_list_1[(int)((long)(chosen_1))])));
open_list_1[(int)((long)(chosen_1))] = ((PQNode[])(pair_1.pq));
            PQNode current_3 = pair_1.node;
            i_14 = 0L;
            while ((long)(i_14) < (long)(n_heuristic)) {
                if ((long)(i_14) != (long)(chosen_1)) {
open_list_1[(int)((long)(i_14))] = ((PQNode[])(pq_remove(((PQNode[])(open_list_1[(int)((long)(i_14))])), current_3.pos)));
                }
                i_14 = (long)((long)(i_14) + 1L);
            }
            String ckey_1 = String.valueOf(pos_key(current_3.pos));
            if (visited_1.containsKey(ckey_1)) {
                continue;
            }
visited_1.put(ckey_1, true);
            if (pos_equal(current_3.pos, goal)) {
                Pos[] path_2 = ((Pos[])(reconstruct(back_pointer_1, goal, start)));
                long j_1 = 0L;
                while ((long)(j_1) < (long)(path_2.length)) {
                    Pos p_1 = path_2[(int)((long)(j_1))];
                    System.out.println("(" + _p(p_1.x) + "," + _p(p_1.y) + ")");
                    j_1 = (long)((long)(j_1) + 1L);
                }
                return;
            }
            Pos[] neighs_1 = ((Pos[])(neighbours(current_3.pos)));
            long k_1 = 0L;
            while ((long)(k_1) < (long)(neighs_1.length)) {
                Pos nb_1 = neighs_1[(int)((long)(k_1))];
                if (valid(nb_1) && ((in_blocks(nb_1) == false))) {
                    String nkey_1 = String.valueOf(pos_key(nb_1));
                    double tentative_1 = (double)((double)(((double)(g_function).getOrDefault(ckey_1, 0.0))) + (double)(1.0));
                    if (!(g_function.containsKey(nkey_1)) || (double)(tentative_1) < (double)(((double)(g_function).getOrDefault(nkey_1, 0.0)))) {
g_function.put(nkey_1, (double)(tentative_1));
back_pointer_1.put(nkey_1, current_3.pos);
                        i_14 = 0L;
                        while ((long)(i_14) < (long)(n_heuristic)) {
                            double pri2_1 = (double)((double)(tentative_1) + (double)((double)(W1) * (double)(heuristic((long)(i_14), nb_1, goal))));
open_list_1[(int)((long)(i_14))] = ((PQNode[])(pq_put(((PQNode[])(open_list_1[(int)((long)(i_14))])), nb_1, (double)(pri2_1))));
                            i_14 = (long)((long)(i_14) + 1L);
                        }
                    }
                }
                k_1 = (long)((long)(k_1) + 1L);
            }
        }
        System.out.println("No path found to goal");
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            blocks = ((Pos[])(new Pos[]{new Pos(0, 1), new Pos(1, 1), new Pos(2, 1), new Pos(3, 1), new Pos(4, 1), new Pos(5, 1), new Pos(6, 1), new Pos(7, 1), new Pos(8, 1), new Pos(9, 1), new Pos(10, 1), new Pos(11, 1), new Pos(12, 1), new Pos(13, 1), new Pos(14, 1), new Pos(15, 1), new Pos(16, 1), new Pos(17, 1), new Pos(18, 1), new Pos(19, 1)}));
            start = new Pos(0, 0);
            goal = new Pos((long)(n) - 1L, (long)(n) - 1L);
            multi_a_star(start, goal, (long)(n_heuristic));
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
