public class Main {
    static class Solution {
        String[] path;
        int cost;
        Solution(String[] path, int cost) {
            this.path = path;
            this.cost = cost;
        }
        Solution() {}
        @Override public String toString() {
            return String.format("{'path': %s, 'cost': %s}", String.valueOf(path), String.valueOf(cost));
        }
    }

    static class Swap {
        String a;
        String b;
        Swap(String a, String b) {
            this.a = a;
            this.b = b;
        }
        Swap() {}
        @Override public String toString() {
            return String.format("{'a': '%s', 'b': '%s'}", String.valueOf(a), String.valueOf(b));
        }
    }

    static java.util.Map<String,java.util.Map<String,Integer>> graph;
    static Solution first;
    static Solution best_1;

    static int path_cost(String[] path, java.util.Map<String,java.util.Map<String,Integer>> graph) {
        int total = 0;
        int i = 0;
        while (i < path.length - 1) {
            String u = path[i];
            String v = path[i + 1];
            total = total + (int)(((int)(((java.util.Map<String,Integer>)(graph).get(u))).getOrDefault(v, 0)));
            i = i + 1;
        }
        return total;
    }

    static Solution generate_first_solution(java.util.Map<String,java.util.Map<String,Integer>> graph, String start) {
        String[] path = ((String[])(new String[]{}));
        String visiting = start;
        int total_1 = 0;
        while (path.length < graph.size()) {
            path = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(visiting)).toArray(String[]::new)));
            String best_node = "";
            int best_cost = 1000000;
            for (String n : ((java.util.Map<String,Integer>)(graph).get(visiting)).keySet()) {
                if (!(java.util.Arrays.asList(path).contains(n)) && (int)(((int)(((java.util.Map<String,Integer>)(graph).get(visiting))).getOrDefault(n, 0))) < best_cost) {
                    best_cost = (int)(((int)(((java.util.Map<String,Integer>)(graph).get(visiting))).getOrDefault(n, 0)));
                    best_node = n;
                }
            }
            if ((best_node.equals(""))) {
                break;
            }
            total_1 = total_1 + best_cost;
            visiting = best_node;
        }
        path = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(path), java.util.stream.Stream.of(start)).toArray(String[]::new)));
        total_1 = total_1 + (int)(((int)(((java.util.Map<String,Integer>)(graph).get(visiting))).getOrDefault(start, 0)));
        return new Solution(path, total_1);
    }

    static String[] copy_path(String[] path) {
        String[] res = ((String[])(new String[]{}));
        int i_1 = 0;
        while (i_1 < path.length) {
            res = ((String[])(java.util.stream.Stream.concat(java.util.Arrays.stream(res), java.util.stream.Stream.of(path[i_1])).toArray(String[]::new)));
            i_1 = i_1 + 1;
        }
        return res;
    }

    static Solution[] find_neighborhood(Solution sol, java.util.Map<String,java.util.Map<String,Integer>> graph) {
        Solution[] neighbors = ((Solution[])(new Solution[]{}));
        int i_2 = 1;
        while (i_2 < sol.path.length - 1) {
            int j = 1;
            while (j < sol.path.length - 1) {
                if (i_2 != j) {
                    String[] new_path = ((String[])(copy_path(((String[])(sol.path)))));
                    String tmp = new_path[i_2];
new_path[i_2] = new_path[j];
new_path[j] = tmp;
                    int cost = path_cost(((String[])(new_path)), graph);
                    neighbors = ((Solution[])(java.util.stream.Stream.concat(java.util.Arrays.stream(neighbors), java.util.stream.Stream.of(new Solution(new_path, cost))).toArray(Solution[]::new)));
                }
                j = j + 1;
            }
            i_2 = i_2 + 1;
        }
        return neighbors;
    }

    static Swap find_swap(String[] a, String[] b) {
        int i_3 = 0;
        while (i_3 < a.length) {
            if (!(a[i_3].equals(b[i_3]))) {
                return new Swap(a[i_3], b[i_3]);
            }
            i_3 = i_3 + 1;
        }
        return new Swap("", "");
    }

    static Solution tabu_search(Solution first, java.util.Map<String,java.util.Map<String,Integer>> graph, int iters, int size) {
        Solution solution = first;
        Solution best = first;
        Swap[] tabu = ((Swap[])(new Swap[]{}));
        int count = 0;
        while (count < iters) {
            Solution[] neighborhood = ((Solution[])(find_neighborhood(solution, graph)));
            if (neighborhood.length == 0) {
                break;
            }
            Solution best_neighbor = neighborhood[0];
            Swap best_move = find_swap(((String[])(solution.path)), ((String[])(best_neighbor.path)));
            int i_4 = 1;
            while (i_4 < neighborhood.length) {
                Solution cand = neighborhood[i_4];
                Swap move = find_swap(((String[])(solution.path)), ((String[])(cand.path)));
                boolean forbidden = false;
                int t = 0;
                while (t < tabu.length) {
                    if (((tabu[t].a.equals(move.a)) && (tabu[t].b.equals(move.b))) || ((tabu[t].a.equals(move.b)) && (tabu[t].b.equals(move.a)))) {
                        forbidden = true;
                    }
                    t = t + 1;
                }
                if (forbidden == false && cand.cost < best_neighbor.cost) {
                    best_neighbor = cand;
                    best_move = move;
                }
                i_4 = i_4 + 1;
            }
            solution = best_neighbor;
            tabu = ((Swap[])(java.util.stream.Stream.concat(java.util.Arrays.stream(tabu), java.util.stream.Stream.of(best_move)).toArray(Swap[]::new)));
            if (tabu.length > size) {
                Swap[] new_tab = ((Swap[])(new Swap[]{}));
                int j_1 = 1;
                while (j_1 < tabu.length) {
                    new_tab = ((Swap[])(java.util.stream.Stream.concat(java.util.Arrays.stream(new_tab), java.util.stream.Stream.of(tabu[j_1])).toArray(Swap[]::new)));
                    j_1 = j_1 + 1;
                }
                tabu = ((Swap[])(new_tab));
            }
            if (solution.cost < best.cost) {
                best = solution;
            }
            count = count + 1;
        }
        return best;
    }
    public static void main(String[] args) {
        graph = ((java.util.Map<String,java.util.Map<String,Integer>>)(new java.util.LinkedHashMap<String, java.util.Map<String,Integer>>(java.util.Map.ofEntries(java.util.Map.entry("a", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("b", 20), java.util.Map.entry("c", 18), java.util.Map.entry("d", 22), java.util.Map.entry("e", 26)))))), java.util.Map.entry("b", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 20), java.util.Map.entry("c", 10), java.util.Map.entry("d", 11), java.util.Map.entry("e", 12)))))), java.util.Map.entry("c", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 18), java.util.Map.entry("b", 10), java.util.Map.entry("d", 23), java.util.Map.entry("e", 24)))))), java.util.Map.entry("d", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 22), java.util.Map.entry("b", 11), java.util.Map.entry("c", 23), java.util.Map.entry("e", 40)))))), java.util.Map.entry("e", ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("a", 26), java.util.Map.entry("b", 12), java.util.Map.entry("c", 24), java.util.Map.entry("d", 40))))))))));
        first = generate_first_solution(graph, "a");
        best_1 = tabu_search(first, graph, 4, 3);
        System.out.println(_p(best_1.path));
        System.out.println(_p(best_1.cost));
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
