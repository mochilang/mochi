public class Main {
    static class SearchProblem {
        double x;
        double y;
        double step;
        java.util.function.BiFunction<Double,Double,Double> f;
        SearchProblem(double x, double y, double step, java.util.function.BiFunction<Double,Double,Double> f) {
            this.x = x;
            this.y = y;
            this.step = step;
            this.f = f;
        }
        SearchProblem() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'step': %s, 'f': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(step), String.valueOf(f));
        }
    }


    static double score(SearchProblem sp) {
        return ((double)(sp.f.apply(sp.x, sp.y)));
    }

    static SearchProblem[] neighbors(SearchProblem sp) {
        double s = sp.step;
        return new SearchProblem[]{new SearchProblem(sp.x - s, sp.y - s, s, sp.f), new SearchProblem(sp.x - s, sp.y, s, sp.f), new SearchProblem(sp.x - s, sp.y + s, s, sp.f), new SearchProblem(sp.x, sp.y - s, s, sp.f), new SearchProblem(sp.x, sp.y + s, s, sp.f), new SearchProblem(sp.x + s, sp.y - s, s, sp.f), new SearchProblem(sp.x + s, sp.y, s, sp.f), new SearchProblem(sp.x + s, sp.y + s, s, sp.f)};
    }

    static boolean equal_state(SearchProblem a, SearchProblem b) {
        return a.x == b.x && a.y == b.y;
    }

    static boolean contains_state(SearchProblem[] lst, SearchProblem sp) {
        long i = 0;
        while (i < lst.length) {
            if (((Boolean)(equal_state(lst[(int)(i)], sp)))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static SearchProblem hill_climbing(SearchProblem sp, boolean find_max, double max_x, double min_x, double max_y, double min_y, long max_iter) {
        SearchProblem current = sp;
        SearchProblem[] visited_1 = ((SearchProblem[])(new SearchProblem[]{}));
        long iterations_1 = 0;
        boolean solution_found_1 = false;
        while (solution_found_1 == false && iterations_1 < max_iter) {
            visited_1 = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited_1), java.util.stream.Stream.of(current)).toArray(SearchProblem[]::new)));
            iterations_1 = iterations_1 + 1;
            double current_score_1 = score(current);
            SearchProblem[] neighs_1 = ((SearchProblem[])(neighbors(current)));
            double max_change_1 = -1000000000000000000.0;
            double min_change_1 = 1000000000000000000.0;
            SearchProblem next_1 = current;
            boolean improved_1 = false;
            long i_2 = 0;
            while (i_2 < neighs_1.length) {
                SearchProblem n_1 = neighs_1[(int)(i_2)];
                i_2 = i_2 + 1;
                if (((Boolean)(contains_state(((SearchProblem[])(visited_1)), n_1)))) {
                    continue;
                }
                if (n_1.x > max_x || n_1.x < min_x || n_1.y > max_y || n_1.y < min_y) {
                    continue;
                }
                double change_1 = score(n_1) - current_score_1;
                if (((Boolean)(find_max))) {
                    if (change_1 > max_change_1 && change_1 > 0.0) {
                        max_change_1 = change_1;
                        next_1 = n_1;
                        improved_1 = true;
                    }
                } else                 if (change_1 < min_change_1 && change_1 < 0.0) {
                    min_change_1 = change_1;
                    next_1 = n_1;
                    improved_1 = true;
                }
            }
            if (improved_1) {
                current = next_1;
            } else {
                solution_found_1 = true;
            }
        }
        return current;
    }

    static double test_f1(double x, double y) {
        return x * x + y * y;
    }

    static void main() {
        SearchProblem prob1 = new SearchProblem(3.0, 4.0, 1.0, Main::test_f1);
        SearchProblem local_min1_1 = hill_climbing(prob1, false, 1000000000.0, -1000000000.0, 1000000000.0, -1000000000.0, 10000);
        System.out.println(_p(((Number)(score(local_min1_1))).intValue()));
        SearchProblem prob2_1 = new SearchProblem(12.0, 47.0, 1.0, Main::test_f1);
        SearchProblem local_min2_1 = hill_climbing(prob2_1, false, 100.0, 5.0, 50.0, -5.0, 10000);
        System.out.println(_p(((Number)(score(local_min2_1))).intValue()));
        SearchProblem prob3_1 = new SearchProblem(3.0, 4.0, 1.0, Main::test_f1);
        SearchProblem local_max_1 = hill_climbing(prob3_1, true, 1000000000.0, -1000000000.0, 1000000000.0, -1000000000.0, 1000);
        System.out.println(_p(((Number)(score(local_max_1))).intValue()));
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
        if (v instanceof Double || v instanceof Float) {
            double d = ((Number) v).doubleValue();
            if (d == Math.rint(d)) return String.valueOf((long) d);
            return String.valueOf(d);
        }
        return String.valueOf(v);
    }
}
