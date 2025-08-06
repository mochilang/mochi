public class Main {
    static int[][] graph;

    static int longest_distance(int[][] graph) {
        int n = graph.length;
        int[] indegree = ((int[])(new int[]{}));
        int i = 0;
        while (i < n) {
            indegree = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(indegree), java.util.stream.IntStream.of(0)).toArray()));
            i = i + 1;
        }
        int[] long_dist = ((int[])(new int[]{}));
        int j = 0;
        while (j < n) {
            long_dist = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(long_dist), java.util.stream.IntStream.of(1)).toArray()));
            j = j + 1;
        }
        int u = 0;
        while (u < n) {
            for (int v : graph[u]) {
indegree[v] = indegree[v] + 1;
            }
            u = u + 1;
        }
        int[] queue = ((int[])(new int[]{}));
        int head = 0;
        int k = 0;
        while (k < n) {
            if (indegree[k] == 0) {
                queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(k)).toArray()));
            }
            k = k + 1;
        }
        while (head < queue.length) {
            int vertex = queue[head];
            head = head + 1;
            for (int x : graph[vertex]) {
indegree[x] = indegree[x] - 1;
                int new_dist = long_dist[vertex] + 1;
                if (new_dist > long_dist[x]) {
long_dist[x] = new_dist;
                }
                if (indegree[x] == 0) {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(x)).toArray()));
                }
            }
        }
        int max_len = long_dist[0];
        int m = 1;
        while (m < n) {
            if (long_dist[m] > max_len) {
                max_len = long_dist[m];
            }
            m = m + 1;
        }
        return max_len;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((int[][])(new int[][]{new int[]{2, 3, 4}, new int[]{2, 7}, new int[]{5}, new int[]{5, 7}, new int[]{7}, new int[]{6}, new int[]{7}, new int[]{}}));
            System.out.println(longest_distance(((int[][])(graph))));
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
