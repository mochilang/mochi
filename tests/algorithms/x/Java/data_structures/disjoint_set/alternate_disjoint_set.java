public class Main {
    static class DisjointSet {
        int[] set_counts;
        int max_set;
        int[] ranks;
        int[] parents;
        DisjointSet(int[] set_counts, int max_set, int[] ranks, int[] parents) {
            this.set_counts = set_counts;
            this.max_set = max_set;
            this.ranks = ranks;
            this.parents = parents;
        }
        DisjointSet() {}
        @Override public String toString() {
            return String.format("{'set_counts': %s, 'max_set': %s, 'ranks': %s, 'parents': %s}", String.valueOf(set_counts), String.valueOf(max_set), String.valueOf(ranks), String.valueOf(parents));
        }
    }

    static DisjointSet ds = null;

    static int max_list(int[] xs) {
        int m = xs[0];
        int i = 1;
        while (i < xs.length) {
            if (xs[i] > m) {
                m = xs[i];
            }
            i = i + 1;
        }
        return m;
    }

    static DisjointSet disjoint_set_new(int[] set_counts) {
        int max_set = max_list(((int[])(set_counts)));
        int num_sets = set_counts.length;
        int[] ranks = ((int[])(new int[]{}));
        int[] parents = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < num_sets) {
            ranks = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(ranks), java.util.stream.IntStream.of(1)).toArray()));
            parents = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(parents), java.util.stream.IntStream.of(i_1)).toArray()));
            i_1 = i_1 + 1;
        }
        return new DisjointSet(set_counts, max_set, ranks, parents);
    }

    static int get_parent(DisjointSet ds, int idx) {
        if (ds.parents[idx] == idx) {
            return idx;
        }
        int[] parents_1 = ((int[])(ds.parents));
parents_1[idx] = get_parent(ds, parents_1[idx]);
ds.parents = parents_1;
        return ds.parents[idx];
    }

    static boolean merge(DisjointSet ds, int src, int dst) {
        int src_parent = get_parent(ds, src);
        int dst_parent = get_parent(ds, dst);
        if (src_parent == dst_parent) {
            return false;
        }
        if (ds.ranks[dst_parent] >= ds.ranks[src_parent]) {
            int[] counts = ((int[])(ds.set_counts));
counts[dst_parent] = counts[dst_parent] + counts[src_parent];
counts[src_parent] = 0;
ds.set_counts = counts;
            int[] parents_2 = ((int[])(ds.parents));
parents_2[src_parent] = dst_parent;
ds.parents = parents_2;
            if (ds.ranks[dst_parent] == ds.ranks[src_parent]) {
                int[] ranks_1 = ((int[])(ds.ranks));
ranks_1[dst_parent] = ranks_1[dst_parent] + 1;
ds.ranks = ranks_1;
            }
            int joined = ds.set_counts[dst_parent];
            if (joined > ds.max_set) {
ds.max_set = joined;
            }
        } else {
            int[] counts_1 = ((int[])(ds.set_counts));
counts_1[src_parent] = counts_1[src_parent] + counts_1[dst_parent];
counts_1[dst_parent] = 0;
ds.set_counts = counts_1;
            int[] parents_3 = ((int[])(ds.parents));
parents_3[dst_parent] = src_parent;
ds.parents = parents_3;
            int joined_1 = ds.set_counts[src_parent];
            if (joined_1 > ds.max_set) {
ds.max_set = joined_1;
            }
        }
        return true;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            ds = disjoint_set_new(((int[])(new int[]{1, 1, 1})));
            System.out.println(merge(ds, 1, 2));
            System.out.println(merge(ds, 0, 2));
            System.out.println(merge(ds, 0, 1));
            System.out.println(get_parent(ds, 0));
            System.out.println(get_parent(ds, 1));
            System.out.println(ds.max_set);
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
