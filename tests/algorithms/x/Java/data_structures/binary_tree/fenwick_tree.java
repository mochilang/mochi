public class Main {
    static class FenwickTree {
        int size;
        int[] tree;
        FenwickTree(int size, int[] tree) {
            this.size = size;
            this.tree = tree;
        }
        FenwickTree() {}
        @Override public String toString() {
            return String.format("{'size': %s, 'tree': %s}", String.valueOf(size), String.valueOf(tree));
        }
    }

    static FenwickTree f_base;
    static FenwickTree f = null;
    static FenwickTree f2;
    static FenwickTree f3;

    static FenwickTree fenwick_from_list(int[] arr) {
        int size = arr.length;
        int[] tree = ((int[])(new int[]{}));
        int i = 0;
        while (i < size) {
            tree = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(tree), java.util.stream.IntStream.of(arr[i])).toArray()));
            i = i + 1;
        }
        i = 1;
        while (i < size) {
            int j = fenwick_next(i);
            if (j < size) {
tree[j] = tree[j] + tree[i];
            }
            i = i + 1;
        }
        return new FenwickTree(size, tree);
    }

    static FenwickTree fenwick_empty(int size) {
        int[] tree_1 = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < size) {
            tree_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(tree_1), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        return new FenwickTree(size, tree_1);
    }

    static int[] fenwick_get_array(FenwickTree f) {
        int[] arr = ((int[])(new int[]{}));
        int i_2 = 0;
        while (i_2 < f.size) {
            arr = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(arr), java.util.stream.IntStream.of(f.tree[i_2])).toArray()));
            i_2 = i_2 + 1;
        }
        i_2 = f.size - 1;
        while (i_2 > 0) {
            int j_1 = fenwick_next(i_2);
            if (j_1 < f.size) {
arr[j_1] = arr[j_1] - arr[i_2];
            }
            i_2 = i_2 - 1;
        }
        return arr;
    }

    static int bit_and(int a, int b) {
        int ua = a;
        int ub = b;
        int res = 0;
        int bit = 1;
        while (ua != 0 || ub != 0) {
            if (Math.floorMod(ua, 2) == 1 && Math.floorMod(ub, 2) == 1) {
                res = res + bit;
            }
            ua = ((Number)((ua / 2))).intValue();
            ub = ((Number)((ub / 2))).intValue();
            bit = bit * 2;
        }
        return res;
    }

    static int low_bit(int x) {
        if (x == 0) {
            return 0;
        }
        return x - bit_and(x, x - 1);
    }

    static int fenwick_next(int index) {
        return index + low_bit(index);
    }

    static int fenwick_prev(int index) {
        return index - low_bit(index);
    }

    static FenwickTree fenwick_add(FenwickTree f, int index, int value) {
        int[] tree_2 = ((int[])(f.tree));
        if (index == 0) {
tree_2[0] = tree_2[0] + value;
            return new FenwickTree(f.size, tree_2);
        }
        int i_3 = index;
        while (i_3 < f.size) {
tree_2[i_3] = tree_2[i_3] + value;
            i_3 = fenwick_next(i_3);
        }
        return new FenwickTree(f.size, tree_2);
    }

    static FenwickTree fenwick_update(FenwickTree f, int index, int value) {
        int current = fenwick_get(f, index);
        return fenwick_add(f, index, value - current);
    }

    static int fenwick_prefix(FenwickTree f, int right) {
        if (right == 0) {
            return 0;
        }
        int result = f.tree[0];
        int r = right - 1;
        while (r > 0) {
            result = result + f.tree[r];
            r = fenwick_prev(r);
        }
        return result;
    }

    static int fenwick_query(FenwickTree f, int left, int right) {
        return fenwick_prefix(f, right) - fenwick_prefix(f, left);
    }

    static int fenwick_get(FenwickTree f, int index) {
        return fenwick_query(f, index, index + 1);
    }

    static int fenwick_rank_query(FenwickTree f, int value) {
        int v = value - f.tree[0];
        if (v < 0) {
            return -1;
        }
        int j_2 = 1;
        while (j_2 * 2 < f.size) {
            j_2 = j_2 * 2;
        }
        int i_4 = 0;
        int jj = j_2;
        while (jj > 0) {
            if (i_4 + jj < f.size && f.tree[i_4 + jj] <= v) {
                v = v - f.tree[i_4 + jj];
                i_4 = i_4 + jj;
            }
            jj = jj / 2;
        }
        return i_4;
    }
    public static void main(String[] args) {
        f_base = fenwick_from_list(((int[])(new int[]{1, 2, 3, 4, 5})));
        System.out.println(fenwick_get_array(f_base));
        f = fenwick_from_list(((int[])(new int[]{1, 2, 3, 4, 5})));
        f = fenwick_add(f, 0, 1);
        f = fenwick_add(f, 1, 2);
        f = fenwick_add(f, 2, 3);
        f = fenwick_add(f, 3, 4);
        f = fenwick_add(f, 4, 5);
        System.out.println(fenwick_get_array(f));
        f2 = fenwick_from_list(((int[])(new int[]{1, 2, 3, 4, 5})));
        System.out.println(fenwick_prefix(f2, 3));
        System.out.println(fenwick_query(f2, 1, 4));
        f3 = fenwick_from_list(((int[])(new int[]{1, 2, 0, 3, 0, 5})));
        System.out.println(fenwick_rank_query(f3, 0));
        System.out.println(fenwick_rank_query(f3, 2));
        System.out.println(fenwick_rank_query(f3, 1));
        System.out.println(fenwick_rank_query(f3, 3));
        System.out.println(fenwick_rank_query(f3, 5));
        System.out.println(fenwick_rank_query(f3, 6));
        System.out.println(fenwick_rank_query(f3, 11));
    }
}
