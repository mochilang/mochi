public class Main {
    static class ExpandResult {
        int[] queue;
        int head;
        java.util.Map<Integer,Integer> parents;
        java.util.Map<Integer,Boolean> visited;
        int intersection;
        boolean found;
        ExpandResult(int[] queue, int head, java.util.Map<Integer,Integer> parents, java.util.Map<Integer,Boolean> visited, int intersection, boolean found) {
            this.queue = queue;
            this.head = head;
            this.parents = parents;
            this.visited = visited;
            this.intersection = intersection;
            this.found = found;
        }
        ExpandResult() {}
        @Override public String toString() {
            return String.format("{'queue': %s, 'head': %s, 'parents': %s, 'visited': %s, 'intersection': %s, 'found': %s}", String.valueOf(queue), String.valueOf(head), String.valueOf(parents), String.valueOf(visited), String.valueOf(intersection), String.valueOf(found));
        }
    }

    static class SearchResult {
        int[] path;
        boolean ok;
        SearchResult(int[] path, boolean ok) {
            this.path = path;
            this.ok = ok;
        }
        SearchResult() {}
        @Override public String toString() {
            return String.format("{'path': %s, 'ok': %s}", String.valueOf(path), String.valueOf(ok));
        }
    }


    static ExpandResult expand_search(java.util.Map<Integer,int[]> graph, int[] queue, int head, java.util.Map<Integer,Integer> parents, java.util.Map<Integer,Boolean> visited, java.util.Map<Integer,Boolean> opposite_visited) {
        if (head >= queue.length) {
            return new ExpandResult(queue, head, parents, visited, 0 - 1, false);
        }
        int current = queue[head];
        head = head + 1;
        int[] neighbors = (int[])(((int[])(graph).get(current)));
        int[] q = ((int[])(queue));
        java.util.Map<Integer,Integer> p = parents;
        java.util.Map<Integer,Boolean> v = visited;
        int i = 0;
        while (i < neighbors.length) {
            int neighbor = neighbors[i];
            if (((boolean)(v).getOrDefault(neighbor, false))) {
                i = i + 1;
                continue;
            }
v.put(neighbor, true);
p.put(neighbor, current);
            q = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(q), java.util.stream.IntStream.of(neighbor)).toArray()));
            if (((boolean)(opposite_visited).getOrDefault(neighbor, false))) {
                return new ExpandResult(q, head, p, v, neighbor, true);
            }
            i = i + 1;
        }
        return new ExpandResult(q, head, p, v, 0 - 1, false);
    }

    static int[] construct_path(int current, java.util.Map<Integer,Integer> parents) {
        int[] path = ((int[])(new int[]{}));
        int node = current;
        while (node != 0 - 1) {
            path = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(path), java.util.stream.IntStream.of(node)).toArray()));
            node = (int)(((int)(parents).getOrDefault(node, 0)));
        }
        return path;
    }

    static int[] reverse_list(int[] xs) {
        int[] res = ((int[])(new int[]{}));
        int i_1 = xs.length;
        while (i_1 > 0) {
            i_1 = i_1 - 1;
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i_1])).toArray()));
        }
        return res;
    }

    static SearchResult bidirectional_search(java.util.Map<Integer,int[]> g, int start, int goal) {
        if (start == goal) {
            return new SearchResult(new int[]{start}, true);
        }
        java.util.Map<Integer,Integer> forward_parents = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
forward_parents.put(start, 0 - 1);
        java.util.Map<Integer,Integer> backward_parents = ((java.util.Map<Integer,Integer>)(new java.util.LinkedHashMap<Integer, Integer>()));
backward_parents.put(goal, 0 - 1);
        java.util.Map<Integer,Boolean> forward_visited = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
forward_visited.put(start, true);
        java.util.Map<Integer,Boolean> backward_visited = ((java.util.Map<Integer,Boolean>)(new java.util.LinkedHashMap<Integer, Boolean>()));
backward_visited.put(goal, true);
        int[] forward_queue = ((int[])(new int[]{start}));
        int[] backward_queue = ((int[])(new int[]{goal}));
        int forward_head = 0;
        int backward_head = 0;
        int intersection = 0 - 1;
        while (forward_head < forward_queue.length && backward_head < backward_queue.length && intersection == 0 - 1) {
            ExpandResult res_1 = expand_search(g, ((int[])(forward_queue)), forward_head, forward_parents, forward_visited, backward_visited);
            forward_queue = ((int[])(res_1.queue));
            forward_head = res_1.head;
            forward_parents = res_1.parents;
            forward_visited = res_1.visited;
            if (res_1.found) {
                intersection = res_1.intersection;
                break;
            }
            res_1 = expand_search(g, ((int[])(backward_queue)), backward_head, backward_parents, backward_visited, forward_visited);
            backward_queue = ((int[])(res_1.queue));
            backward_head = res_1.head;
            backward_parents = res_1.parents;
            backward_visited = res_1.visited;
            if (res_1.found) {
                intersection = res_1.intersection;
                break;
            }
        }
        if (intersection == 0 - 1) {
            return new SearchResult(new int[]{}, false);
        }
        int[] forward_path = ((int[])(construct_path(intersection, forward_parents)));
        forward_path = ((int[])(reverse_list(((int[])(forward_path)))));
        int back_start = (int)(((int)(backward_parents).getOrDefault(intersection, 0)));
        int[] backward_path = ((int[])(construct_path(back_start, backward_parents)));
        int[] result = ((int[])(forward_path));
        int j = 0;
        while (j < backward_path.length) {
            result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(backward_path[j])).toArray()));
            j = j + 1;
        }
        return new SearchResult(result, true);
    }

    static boolean is_edge(java.util.Map<Integer,int[]> g, int u, int v) {
        int[] neighbors_1 = (int[])(((int[])(g).get(u)));
        int i_2 = 0;
        while (i_2 < neighbors_1.length) {
            if (neighbors_1[i_2] == v) {
                return true;
            }
            i_2 = i_2 + 1;
        }
        return false;
    }

    static boolean path_exists(java.util.Map<Integer,int[]> g, int[] path) {
        if (path.length == 0) {
            return false;
        }
        int i_3 = 0;
        while (i_3 + 1 < path.length) {
            if (!(Boolean)is_edge(g, path[i_3], path[i_3 + 1])) {
                return false;
            }
            i_3 = i_3 + 1;
        }
        return true;
    }

    static void print_path(java.util.Map<Integer,int[]> g, int s, int t) {
        SearchResult res_2 = bidirectional_search(g, s, t);
        if (res_2.ok && ((Boolean)(path_exists(g, ((int[])(res_2.path)))))) {
            System.out.println("Path from " + _p(s) + " to " + _p(t) + ": " + _p(res_2.path));
        } else {
            System.out.println("Path from " + _p(s) + " to " + _p(t) + ": None");
        }
    }

    static void main() {
        java.util.Map<Integer,int[]> graph = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 2}))), java.util.Map.entry(1, ((int[])(new int[]{0, 3, 4}))), java.util.Map.entry(2, ((int[])(new int[]{0, 5, 6}))), java.util.Map.entry(3, ((int[])(new int[]{1, 7}))), java.util.Map.entry(4, ((int[])(new int[]{1, 8}))), java.util.Map.entry(5, ((int[])(new int[]{2, 9}))), java.util.Map.entry(6, ((int[])(new int[]{2, 10}))), java.util.Map.entry(7, ((int[])(new int[]{3, 11}))), java.util.Map.entry(8, ((int[])(new int[]{4, 11}))), java.util.Map.entry(9, ((int[])(new int[]{5, 11}))), java.util.Map.entry(10, ((int[])(new int[]{6, 11}))), java.util.Map.entry(11, ((int[])(new int[]{7, 8, 9, 10})))))));
        print_path(graph, 0, 11);
        print_path(graph, 5, 5);
        java.util.Map<Integer,int[]> disconnected = ((java.util.Map<Integer,int[]>)(new java.util.LinkedHashMap<Integer, int[]>(java.util.Map.ofEntries(java.util.Map.entry(0, ((int[])(new int[]{1, 2}))), java.util.Map.entry(1, ((int[])(new int[]{0}))), java.util.Map.entry(2, ((int[])(new int[]{0}))), java.util.Map.entry(3, ((int[])(new int[]{4}))), java.util.Map.entry(4, ((int[])(new int[]{3})))))));
        print_path(disconnected, 0, 3);
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
        return String.valueOf(v);
    }
}
