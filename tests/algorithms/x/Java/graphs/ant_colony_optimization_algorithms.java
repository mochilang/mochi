public class Main {
    static java.util.Map<Integer,int[]> cities;

    static double sqrtApprox(double x) {
        double guess = x / 2.0;
        int i = 0;
        while (i < 20) {
            guess = (guess + x / guess) / 2.0;
            i = i + 1;
        }
        return guess;
    }

    static double rand_float() {
        return ((Number)((Math.floorMod(_now(), 1000000)))).doubleValue() / 1000000.0;
    }

    static double pow_float(double base, double exp) {
        double result = 1.0;
        int i_1 = 0;
        int e = ((Number)(exp)).intValue();
        while (i_1 < e) {
            result = result * base;
            i_1 = i_1 + 1;
        }
        return result;
    }

    static double distance(int[] city1, int[] city2) {
        double dx = ((Number)((city1[0] - city2[0]))).doubleValue();
        double dy = ((Number)((city1[1] - city2[1]))).doubleValue();
        return sqrtApprox(dx * dx + dy * dy);
    }

    static int choose_weighted(int[] options, double[] weights) {
        double total = 0.0;
        int i_2 = 0;
        while (i_2 < weights.length) {
            total = total + weights[i_2];
            i_2 = i_2 + 1;
        }
        double r = rand_float() * total;
        double accum = 0.0;
        i_2 = 0;
        while (i_2 < weights.length) {
            accum = accum + weights[i_2];
            if (r <= accum) {
                return options[i_2];
            }
            i_2 = i_2 + 1;
        }
        return options[options.length - 1];
    }

    static int city_select(double[][] pheromone, int current, int[] unvisited, double alpha, double beta, java.util.Map<Integer,int[]> cities) {
        double[] probs = ((double[])(new double[]{}));
        int i_3 = 0;
        while (i_3 < unvisited.length) {
            int city = unvisited[i_3];
            double dist = distance((int[])(((int[])(cities).get(city))), (int[])(((int[])(cities).get(current))));
            double trail = pheromone[city][current];
            double prob = pow_float(trail, alpha) * pow_float(1.0 / dist, beta);
            probs = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(probs), java.util.stream.DoubleStream.of(prob)).toArray()));
            i_3 = i_3 + 1;
        }
        return choose_weighted(((int[])(unvisited)), ((double[])(probs)));
    }

    static double[][] pheromone_update(double[][] pheromone, java.util.Map<Integer,int[]> cities, double evaporation, int[][] ants_route, double q) {
        int n = pheromone.length;
        int i_4 = 0;
        while (i_4 < n) {
            int j = 0;
            while (j < n) {
pheromone[i_4][j] = pheromone[i_4][j] * evaporation;
                j = j + 1;
            }
            i_4 = i_4 + 1;
        }
        int a = 0;
        while (a < ants_route.length) {
            int[] route = ((int[])(ants_route[a]));
            double total_1 = 0.0;
            int r_1 = 0;
            while (r_1 < route.length - 1) {
                total_1 = total_1 + distance((int[])(((int[])(cities).get(route[r_1]))), (int[])(((int[])(cities).get(route[r_1 + 1]))));
                r_1 = r_1 + 1;
            }
            double delta = q / total_1;
            r_1 = 0;
            while (r_1 < route.length - 1) {
                int u = route[r_1];
                int v = route[r_1 + 1];
pheromone[u][v] = pheromone[u][v] + delta;
pheromone[v][u] = pheromone[u][v];
                r_1 = r_1 + 1;
            }
            a = a + 1;
        }
        return pheromone;
    }

    static int[] remove_value(int[] lst, int val) {
        int[] res = ((int[])(new int[]{}));
        int i_5 = 0;
        while (i_5 < lst.length) {
            if (lst[i_5] != val) {
                res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(lst[i_5])).toArray()));
            }
            i_5 = i_5 + 1;
        }
        return res;
    }

    static void ant_colony(java.util.Map<Integer,int[]> cities, int ants_num, int iterations, double evaporation, double alpha, double beta, double q) {
        int n_1 = cities.size();
        double[][] pheromone = ((double[][])(new double[][]{}));
        int i_6 = 0;
        while (i_6 < n_1) {
            double[] row = ((double[])(new double[]{}));
            int j_1 = 0;
            while (j_1 < n_1) {
                row = ((double[])(java.util.stream.DoubleStream.concat(java.util.Arrays.stream(row), java.util.stream.DoubleStream.of(1.0)).toArray()));
                j_1 = j_1 + 1;
            }
            pheromone = ((double[][])(appendObj(pheromone, row)));
            i_6 = i_6 + 1;
        }
        int[] best_path = ((int[])(new int[]{}));
        double best_distance = 1000000000.0;
        int iter = 0;
        while (iter < iterations) {
            int[][] ants_route = ((int[][])(new int[][]{}));
            int k = 0;
            while (k < ants_num) {
                int[] route_1 = ((int[])(new int[]{0}));
                int[] unvisited = ((int[])(new int[]{}));
                for (int key : cities.keySet()) {
                    if (((Number)(key)).intValue() != 0) {
                        unvisited = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(unvisited), java.util.stream.IntStream.of(((Number)(key)).intValue())).toArray()));
                    }
                }
                int current = 0;
                while (unvisited.length > 0) {
                    int next_city = city_select(((double[][])(pheromone)), current, ((int[])(unvisited)), alpha, beta, cities);
                    route_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(route_1), java.util.stream.IntStream.of(next_city)).toArray()));
                    unvisited = ((int[])(remove_value(((int[])(unvisited)), next_city)));
                    current = next_city;
                }
                route_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(route_1), java.util.stream.IntStream.of(0)).toArray()));
                ants_route = ((int[][])(appendObj(ants_route, route_1)));
                k = k + 1;
            }
            pheromone = ((double[][])(pheromone_update(((double[][])(pheromone)), cities, evaporation, ((int[][])(ants_route)), q)));
            int a_1 = 0;
            while (a_1 < ants_route.length) {
                int[] route_2 = ((int[])(ants_route[a_1]));
                double dist_1 = 0.0;
                int r_2 = 0;
                while (r_2 < route_2.length - 1) {
                    dist_1 = dist_1 + distance((int[])(((int[])(cities).get(route_2[r_2]))), (int[])(((int[])(cities).get(route_2[r_2 + 1]))));
                    r_2 = r_2 + 1;
                }
                if (dist_1 < best_distance) {
                    best_distance = dist_1;
                    best_path = ((int[])(route_2));
                }
                a_1 = a_1 + 1;
            }
            iter = iter + 1;
        }
        System.out.println("best_path = " + _p(best_path));
        System.out.println("best_distance = " + _p(best_distance));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            cities = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{0, 0}))), java.util.Map.entry(1, ((int[])(new int[]{0, 5}))), java.util.Map.entry(2, ((int[])(new int[]{3, 8}))), java.util.Map.entry(3, ((int[])(new int[]{8, 10}))), java.util.Map.entry(4, ((int[])(new int[]{12, 8}))), java.util.Map.entry(5, ((int[])(new int[]{12, 4}))), java.util.Map.entry(6, ((int[])(new int[]{8, 0}))), java.util.Map.entry(7, ((int[])(new int[]{6, 2})))))));
            ant_colony(cities, 10, 20, 0.7, 1.0, 5.0, 10.0);
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
