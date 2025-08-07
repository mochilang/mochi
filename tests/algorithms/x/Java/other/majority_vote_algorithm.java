public class Main {

    static int index_of(int[] xs, int x) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == x) {
                return i;
            }
            i = i + 1;
        }
        return 0 - 1;
    }

    static int[] majority_vote(int[] votes, int votes_needed_to_win) {
        if (votes_needed_to_win < 2) {
            return new int[]{};
        }
        int[] candidates = ((int[])(new int[]{}));
        int[] counts = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < votes.length) {
            int v = votes[i_1];
            int idx = index_of(((int[])(candidates)), v);
            if (idx != 0 - 1) {
counts[idx] = counts[idx] + 1;
            } else             if (candidates.length < votes_needed_to_win - 1) {
                candidates = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(candidates), java.util.stream.IntStream.of(v)).toArray()));
                counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(counts), java.util.stream.IntStream.of(1)).toArray()));
            } else {
                int j = 0;
                while (j < counts.length) {
counts[j] = counts[j] - 1;
                    j = j + 1;
                }
                int[] new_candidates = ((int[])(new int[]{}));
                int[] new_counts = ((int[])(new int[]{}));
                j = 0;
                while (j < candidates.length) {
                    if (counts[j] > 0) {
                        new_candidates = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_candidates), java.util.stream.IntStream.of(candidates[j])).toArray()));
                        new_counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(new_counts), java.util.stream.IntStream.of(counts[j])).toArray()));
                    }
                    j = j + 1;
                }
                candidates = ((int[])(new_candidates));
                counts = ((int[])(new_counts));
            }
            i_1 = i_1 + 1;
        }
        int[] final_counts = ((int[])(new int[]{}));
        int j_1 = 0;
        while (j_1 < candidates.length) {
            final_counts = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(final_counts), java.util.stream.IntStream.of(0)).toArray()));
            j_1 = j_1 + 1;
        }
        i_1 = 0;
        while (i_1 < votes.length) {
            int v_1 = votes[i_1];
            int idx_1 = index_of(((int[])(candidates)), v_1);
            if (idx_1 != 0 - 1) {
final_counts[idx_1] = final_counts[idx_1] + 1;
            }
            i_1 = i_1 + 1;
        }
        int[] result = ((int[])(new int[]{}));
        j_1 = 0;
        while (j_1 < candidates.length) {
            if (final_counts[j_1] * votes_needed_to_win > votes.length) {
                result = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(result), java.util.stream.IntStream.of(candidates[j_1])).toArray()));
            }
            j_1 = j_1 + 1;
        }
        return result;
    }

    static void main() {
        int[] votes = ((int[])(new int[]{1, 2, 2, 3, 1, 3, 2}));
        System.out.println(_p(majority_vote(((int[])(votes)), 3)));
        System.out.println(_p(majority_vote(((int[])(votes)), 2)));
        System.out.println(_p(majority_vote(((int[])(votes)), 4)));
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
