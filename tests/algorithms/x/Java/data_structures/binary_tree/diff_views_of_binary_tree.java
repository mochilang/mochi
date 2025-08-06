public class Main {
    static class Tree {
        int[] values;
        int[] lefts;
        int[] rights;
        int root;
        Tree(int[] values, int[] lefts, int[] rights, int root) {
            this.values = values;
            this.lefts = lefts;
            this.rights = rights;
            this.root = root;
        }
        Tree() {}
        @Override public String toString() {
            return String.format("{'values': %s, 'lefts': %s, 'rights': %s, 'root': %s}", String.valueOf(values), String.valueOf(lefts), String.valueOf(rights), String.valueOf(root));
        }
    }

    static int NIL;
    static class Pair {
        int idx;
        int hd;
        Pair(int idx, int hd) {
            this.idx = idx;
            this.hd = hd;
        }
        Pair() {}
        @Override public String toString() {
            return String.format("{'idx': %s, 'hd': %s}", String.valueOf(idx), String.valueOf(hd));
        }
    }

    static Tree tree;

    static Tree make_tree() {
        return new Tree(new int[]{3, 9, 20, 15, 7}, new int[]{1, NIL, 3, NIL, NIL}, new int[]{2, NIL, 4, NIL, NIL}, 0);
    }

    static int index_of(int[] xs, int x) {
        int i = 0;
        while (i < xs.length) {
            if (xs[i] == x) {
                return i;
            }
            i = i + 1;
        }
        return NIL;
    }

    static void sort_pairs(int[] hds, int[] vals) {
        int i_1 = 0;
        while (i_1 < hds.length) {
            int j = i_1;
            while (j > 0 && hds[j - 1] > hds[j]) {
                int hd_tmp = hds[j - 1];
hds[j - 1] = hds[j];
hds[j] = hd_tmp;
                int val_tmp = vals[j - 1];
vals[j - 1] = vals[j];
vals[j] = val_tmp;
                j = j - 1;
            }
            i_1 = i_1 + 1;
        }
    }

    static int[] right_view(Tree t) {
        int[] res = ((int[])(new int[]{}));
        int[] queue = ((int[])(new int[]{t.root}));
        while (queue.length > 0) {
            int size = queue.length;
            int i_2 = 0;
            while (i_2 < size) {
                int idx = queue[i_2];
                if (t.lefts[idx] != NIL) {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(t.lefts[idx])).toArray()));
                }
                if (t.rights[idx] != NIL) {
                    queue = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue), java.util.stream.IntStream.of(t.rights[idx])).toArray()));
                }
                i_2 = i_2 + 1;
            }
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(t.values[queue[size - 1]])).toArray()));
            queue = ((int[])(java.util.Arrays.copyOfRange(queue, size, queue.length)));
        }
        return res;
    }

    static int[] left_view(Tree t) {
        int[] res_1 = ((int[])(new int[]{}));
        int[] queue_1 = ((int[])(new int[]{t.root}));
        while (queue_1.length > 0) {
            int size_1 = queue_1.length;
            int i_3 = 0;
            while (i_3 < size_1) {
                int idx_1 = queue_1[i_3];
                if (t.lefts[idx_1] != NIL) {
                    queue_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.IntStream.of(t.lefts[idx_1])).toArray()));
                }
                if (t.rights[idx_1] != NIL) {
                    queue_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_1), java.util.stream.IntStream.of(t.rights[idx_1])).toArray()));
                }
                i_3 = i_3 + 1;
            }
            res_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res_1), java.util.stream.IntStream.of(t.values[queue_1[0]])).toArray()));
            queue_1 = ((int[])(java.util.Arrays.copyOfRange(queue_1, size_1, queue_1.length)));
        }
        return res_1;
    }

    static int[] top_view(Tree t) {
        int[] hds = ((int[])(new int[]{}));
        int[] vals = ((int[])(new int[]{}));
        int[] queue_idx = ((int[])(new int[]{t.root}));
        int[] queue_hd = ((int[])(new int[]{0}));
        while (queue_idx.length > 0) {
            int idx_2 = queue_idx[0];
            queue_idx = ((int[])(java.util.Arrays.copyOfRange(queue_idx, 1, queue_idx.length)));
            int hd = queue_hd[0];
            queue_hd = ((int[])(java.util.Arrays.copyOfRange(queue_hd, 1, queue_hd.length)));
            if (index_of(((int[])(hds)), hd) == NIL) {
                hds = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(hds), java.util.stream.IntStream.of(hd)).toArray()));
                vals = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(vals), java.util.stream.IntStream.of(t.values[idx_2])).toArray()));
            }
            if (t.lefts[idx_2] != NIL) {
                queue_idx = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_idx), java.util.stream.IntStream.of(t.lefts[idx_2])).toArray()));
                queue_hd = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_hd), java.util.stream.IntStream.of(hd - 1)).toArray()));
            }
            if (t.rights[idx_2] != NIL) {
                queue_idx = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_idx), java.util.stream.IntStream.of(t.rights[idx_2])).toArray()));
                queue_hd = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_hd), java.util.stream.IntStream.of(hd + 1)).toArray()));
            }
        }
        sort_pairs(((int[])(hds)), ((int[])(vals)));
        return vals;
    }

    static int[] bottom_view(Tree t) {
        int[] hds_1 = ((int[])(new int[]{}));
        int[] vals_1 = ((int[])(new int[]{}));
        int[] queue_idx_1 = ((int[])(new int[]{t.root}));
        int[] queue_hd_1 = ((int[])(new int[]{0}));
        while (queue_idx_1.length > 0) {
            int idx_3 = queue_idx_1[0];
            queue_idx_1 = ((int[])(java.util.Arrays.copyOfRange(queue_idx_1, 1, queue_idx_1.length)));
            int hd_1 = queue_hd_1[0];
            queue_hd_1 = ((int[])(java.util.Arrays.copyOfRange(queue_hd_1, 1, queue_hd_1.length)));
            int pos = index_of(((int[])(hds_1)), hd_1);
            if (pos == NIL) {
                hds_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(hds_1), java.util.stream.IntStream.of(hd_1)).toArray()));
                vals_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(vals_1), java.util.stream.IntStream.of(t.values[idx_3])).toArray()));
            } else {
vals_1[pos] = t.values[idx_3];
            }
            if (t.lefts[idx_3] != NIL) {
                queue_idx_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_idx_1), java.util.stream.IntStream.of(t.lefts[idx_3])).toArray()));
                queue_hd_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_hd_1), java.util.stream.IntStream.of(hd_1 - 1)).toArray()));
            }
            if (t.rights[idx_3] != NIL) {
                queue_idx_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_idx_1), java.util.stream.IntStream.of(t.rights[idx_3])).toArray()));
                queue_hd_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(queue_hd_1), java.util.stream.IntStream.of(hd_1 + 1)).toArray()));
            }
        }
        sort_pairs(((int[])(hds_1)), ((int[])(vals_1)));
        return vals_1;
    }
    public static void main(String[] args) {
        NIL = 0 - 1;
        tree = make_tree();
        System.out.println(right_view(tree));
        System.out.println(left_view(tree));
        System.out.println(top_view(tree));
        System.out.println(bottom_view(tree));
    }
}
