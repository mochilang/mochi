public class Main {
    static class SearchProblem {
        double x;
        double y;
        double step;
        SearchProblem(double x, double y, double step) {
            this.x = x;
            this.y = y;
            this.step = step;
        }
        SearchProblem() {}
        @Override public String toString() {
            return String.format("{'x': %s, 'y': %s, 'step': %s}", String.valueOf(x), String.valueOf(y), String.valueOf(step));
        }
    }

    static int seed = 0;

    static double score(SearchProblem p, java.util.function.BiFunction<Double,Double,Double> f) {
        return f.apply(p.x, p.y);
    }

    static SearchProblem[] get_neighbors(SearchProblem p) {
        double s = p.step;
        SearchProblem[] ns = ((SearchProblem[])(new SearchProblem[]{}));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x - s, p.y - s, s))).toArray(SearchProblem[]::new)));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x - s, p.y, s))).toArray(SearchProblem[]::new)));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x - s, p.y + s, s))).toArray(SearchProblem[]::new)));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x, p.y - s, s))).toArray(SearchProblem[]::new)));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x, p.y + s, s))).toArray(SearchProblem[]::new)));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x + s, p.y - s, s))).toArray(SearchProblem[]::new)));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x + s, p.y, s))).toArray(SearchProblem[]::new)));
        ns = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(ns), java.util.stream.Stream.of(new SearchProblem(p.x + s, p.y + s, s))).toArray(SearchProblem[]::new)));
        return ns;
    }

    static SearchProblem[] remove_at(SearchProblem[] lst, int idx) {
        SearchProblem[] res = ((SearchProblem[])(new SearchProblem[]{}));
        int i = 0;
        while (i < lst.length) {
            if (i != idx) {
                res = ((SearchProblem[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(lst[i])).toArray(SearchProblem[]::new)));
            }
            i = i + 1;
        }
        return res;
    }

    static int rand() {
        int _t = _now();
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return seed;
    }

    static double random_float() {
        return (((Number)(rand())).doubleValue()) / 2147483648.0;
    }

    static int randint(int low, int high) {
        return (Math.floorMod(rand(), (high - low + 1))) + low;
    }

    static double expApprox(double x) {
        double y = x;
        boolean is_neg = false;
        if (x < 0.0) {
            is_neg = true;
            y = -x;
        }
        double term = 1.0;
        double sum = 1.0;
        int n = 1;
        while (n < 30) {
            term = term * y / (((Number)(n)).doubleValue());
            sum = sum + term;
            n = n + 1;
        }
        if (((Boolean)(is_neg))) {
            return 1.0 / sum;
        }
        return sum;
    }

    static SearchProblem simulated_annealing(SearchProblem search_prob, java.util.function.BiFunction<Double,Double,Double> f, boolean find_max, double max_x, double min_x, double max_y, double min_y, double start_temp, double rate_of_decrease, double threshold_temp) {
        boolean search_end = false;
        SearchProblem current_state = search_prob;
        double current_temp = start_temp;
        SearchProblem best_state = current_state;
        while (!(Boolean)search_end) {
            double current_score = score(current_state, f);
            if (score(best_state, f) < current_score) {
                best_state = current_state;
            }
            SearchProblem next_state = current_state;
            boolean found_next = false;
            SearchProblem[] neighbors = ((SearchProblem[])(get_neighbors(current_state)));
            while (!(Boolean)found_next && neighbors.length > 0) {
                int idx = randint(0, neighbors.length - 1);
                SearchProblem picked_neighbor = neighbors[idx];
                neighbors = ((SearchProblem[])(remove_at(((SearchProblem[])(neighbors)), idx)));
                if (picked_neighbor.x > max_x || picked_neighbor.x < min_x || picked_neighbor.y > max_y || picked_neighbor.y < min_y) {
                    continue;
                }
                double change = score(picked_neighbor, f) - current_score;
                if (!(Boolean)find_max) {
                    change = -change;
                }
                if (change > 0.0) {
                    next_state = picked_neighbor;
                    found_next = true;
                } else {
                    double probability = expApprox(change / current_temp);
                    if (random_float() < probability) {
                        next_state = picked_neighbor;
                        found_next = true;
                    }
                }
            }
            current_temp = current_temp - (current_temp * rate_of_decrease);
            if (current_temp < threshold_temp || (!(Boolean)found_next)) {
                search_end = true;
            } else {
                current_state = next_state;
            }
        }
        return best_state;
    }

    static double test_f1(double x, double y) {
        return x * x + y * y;
    }

    static double test_f2(double x, double y) {
        return (3.0 * x * x) - (6.0 * y);
    }

    static void main() {
        SearchProblem prob1 = new SearchProblem(12.0, 47.0, 1.0);
        SearchProblem min_state = simulated_annealing(prob1, Main::test_f1, false, 100.0, 5.0, 50.0, -5.0, 100.0, 0.01, 1.0);
        System.out.println("min1" + " " + String.valueOf(test_f1(min_state.x, min_state.y)));
        SearchProblem prob2 = new SearchProblem(12.0, 47.0, 1.0);
        SearchProblem max_state = simulated_annealing(prob2, Main::test_f1, true, 100.0, 5.0, 50.0, -5.0, 100.0, 0.01, 1.0);
        System.out.println("max1" + " " + String.valueOf(test_f1(max_state.x, max_state.y)));
        SearchProblem prob3 = new SearchProblem(3.0, 4.0, 1.0);
        SearchProblem min_state2 = simulated_annealing(prob3, Main::test_f2, false, 1000.0, -1000.0, 1000.0, -1000.0, 100.0, 0.01, 1.0);
        System.out.println("min2" + " " + String.valueOf(test_f2(min_state2.x, min_state2.y)));
        SearchProblem prob4 = new SearchProblem(3.0, 4.0, 1.0);
        SearchProblem max_state2 = simulated_annealing(prob4, Main::test_f2, true, 1000.0, -1000.0, 1000.0, -1000.0, 100.0, 0.01, 1.0);
        System.out.println("max2" + " " + String.valueOf(test_f2(max_state2.x, max_state2.y)));
    }
    public static void main(String[] args) {
        seed = 1;
        main();
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
}
