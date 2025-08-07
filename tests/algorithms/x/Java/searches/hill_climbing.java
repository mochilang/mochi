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
        return sp.f(sp.x, sp.y);
    }

    static SearchProblem[] neighbors(SearchProblem sp) {
        double s = sp.step;
        return new SearchProblem[]{new SearchProblem(sp.x - s, sp.y - s, s, sp.f), new SearchProblem(sp.x - s, sp.y, s, sp.f), new SearchProblem(sp.x - s, sp.y + s, s, sp.f), new SearchProblem(sp.x, sp.y - s, s, sp.f), new SearchProblem(sp.x, sp.y + s, s, sp.f), new SearchProblem(sp.x + s, sp.y - s, s, sp.f), new SearchProblem(sp.x + s, sp.y, s, sp.f), new SearchProblem(sp.x + s, sp.y + s, s, sp.f)};
    }

    static boolean equal_state(SearchProblem a, SearchProblem b) {
        return a.x == b.x && a.y == b.y;
    }

    static boolean contains_state(SearchProblem[] lst, SearchProblem sp) {
        int i = 0;
        while (i < lst.length) {
            if (((Boolean)(equal_state(lst[i], sp)))) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    static SearchProblem hill_climbing(SearchProblem sp, boolean find_max, double max_x, double min_x, double max_y, double min_y, int max_iter) {
        SearchProblem current = sp;
        SearchProblem[] visited = ((SearchProblem[])(new SearchProblem[]{}));
        int iterations = 0;
        boolean solution_found = false;
        while (solution_found == false && iterations < max_iter) {
            visited = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(visited), java.util.stream.Stream.of(current)).toArray(SearchProblem[]::new)));
            iterations = iterations + 1;
            double current_score = score(current);
            SearchProblem[] neighs = ((SearchProblem[])(neighbors(current)));
            double max_change = -1.0;
            e18;
            double min_change = 1.0;
            e18;
            SearchProblem next = current;
            boolean improved = false;
            int i_1 = 0;
            while (i_1 < neighs.length) {
                SearchProblem n = neighs[i_1];
                i_1 = i_1 + 1;
                if (((Boolean)(contains_state(((SearchProblem[])(visited)), n)))) {
                    continue;
                }
                if (n.x > max_x || n.x < min_x || n.y > max_y || n.y < min_y) {
                    continue;
                }
                double change = score(n) - current_score;
                if (((Boolean)(find_max))) {
                    if (change > max_change && change > 0.0) {
                        max_change = change;
                        next = n;
                        improved = true;
                    }
                } else                 if (change < min_change && change < 0.0) {
                    min_change = change;
                    next = n;
                    improved = true;
                }
            }
            if (improved) {
                current = next;
            } else {
                solution_found = true;
            }
        }
        return current;
    }

    static double test_f1(double x, double y) {
        return x * x + y * y;
    }

    static void main() {
        SearchProblem prob1 = new SearchProblem(3.0, 4.0, 1.0, test_f1);
        SearchProblem local_min1 = hill_climbing(prob1, false, 1000000000.0, -1000000000.0, 1000000000.0, -1000000000.0, 10000);
        System.out.println(_p(((Number)(score(local_min1))).intValue()));
        SearchProblem prob2 = new SearchProblem(12.0, 47.0, 1.0, test_f1);
        SearchProblem local_min2 = hill_climbing(prob2, false, 100.0, 5.0, 50.0, -5.0, 10000);
        System.out.println(_p(((Number)(score(local_min2))).intValue()));
        SearchProblem prob3 = new SearchProblem(3.0, 4.0, 1.0, test_f1);
        SearchProblem local_max = hill_climbing(prob3, true, 1000000000.0, -1000000000.0, 1000000000.0, -1000000000.0, 1000);
        System.out.println(_p(((Number)(score(local_max))).intValue()));
    }
    public static void main(String[] args) {
        main();
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
