public class Main {
    static long[][] graph;

    static long longest_distance(long[][] graph) {
        long n = (long)(graph.length);
        long[] indegree_1 = ((long[])(new long[]{}));
        long i_1 = 0L;
        while ((long)(i_1) < (long)(n)) {
            indegree_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(indegree_1), java.util.stream.LongStream.of(0L)).toArray()));
            i_1 = (long)((long)(i_1) + 1L);
        }
        long[] long_dist_1 = ((long[])(new long[]{}));
        long j_1 = 0L;
        while ((long)(j_1) < (long)(n)) {
            long_dist_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(long_dist_1), java.util.stream.LongStream.of(1L)).toArray()));
            j_1 = (long)((long)(j_1) + 1L);
        }
        long u_1 = 0L;
        while ((long)(u_1) < (long)(n)) {
            for (long v : graph[(int)((long)(u_1))]) {
indegree_1[(int)((long)(v))] = (long)((long)(indegree_1[(int)((long)(v))]) + 1L);
            }
            u_1 = (long)((long)(u_1) + 1L);
        }
        long[] queue_1 = ((long[])(new long[]{}));
        long head_1 = 0L;
        long k_1 = 0L;
        while ((long)(k_1) < (long)(n)) {
            if ((long)(indegree_1[(int)((long)(k_1))]) == 0L) {
                queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(k_1))).toArray()));
            }
            k_1 = (long)((long)(k_1) + 1L);
        }
        while ((long)(head_1) < (long)(queue_1.length)) {
            long vertex_1 = (long)(queue_1[(int)((long)(head_1))]);
            head_1 = (long)((long)(head_1) + 1L);
            for (long x : graph[(int)((long)(vertex_1))]) {
indegree_1[(int)((long)(x))] = (long)((long)(indegree_1[(int)((long)(x))]) - 1L);
                long new_dist_1 = (long)((long)(long_dist_1[(int)((long)(vertex_1))]) + 1L);
                if ((long)(new_dist_1) > (long)(long_dist_1[(int)((long)(x))])) {
long_dist_1[(int)((long)(x))] = (long)(new_dist_1);
                }
                if ((long)(indegree_1[(int)((long)(x))]) == 0L) {
                    queue_1 = ((long[])(java.util.stream.LongStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.LongStream.of((long)(x))).toArray()));
                }
            }
        }
        long max_len_1 = (long)(long_dist_1[(int)((long)(0))]);
        long m_1 = 1L;
        while ((long)(m_1) < (long)(n)) {
            if ((long)(long_dist_1[(int)((long)(m_1))]) > (long)(max_len_1)) {
                max_len_1 = (long)(long_dist_1[(int)((long)(m_1))]);
            }
            m_1 = (long)((long)(m_1) + 1L);
        }
        return max_len_1;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            graph = ((long[][])(new long[][]{new long[]{2, 3, 4}, new long[]{2, 7}, new long[]{5}, new long[]{5, 7}, new long[]{7}, new long[]{6}, new long[]{7}, new long[]{}}));
            System.out.println(longest_distance(((long[][])(graph))));
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
