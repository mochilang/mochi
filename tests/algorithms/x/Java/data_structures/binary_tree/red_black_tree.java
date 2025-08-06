public class Main {
    static int LABEL;
    static int COLOR;
    static int PARENT;
    static int LEFT;
    static int RIGHT;
    static int NEG_ONE;
    static class RBTree {
        int[][] nodes;
        int root;
        RBTree(int[][] nodes, int root) {
            this.nodes = nodes;
            this.root = root;
        }
        RBTree() {}
        @Override public String toString() {
            return String.format("{'nodes': %s, 'root': %s}", String.valueOf(nodes), String.valueOf(root));
        }
    }


    static RBTree make_tree() {
        return new RBTree(new int[][]{}, -1);
    }

    static RBTree rotate_left(RBTree t, int x) {
        int[][] nodes = ((int[][])(t.nodes));
        int y = nodes[x][RIGHT];
        int yLeft = nodes[y][LEFT];
nodes[x][RIGHT] = yLeft;
        if (yLeft != NEG_ONE) {
nodes[yLeft][PARENT] = x;
        }
        int xParent = nodes[x][PARENT];
nodes[y][PARENT] = xParent;
        if (xParent == NEG_ONE) {
t.root = y;
        } else         if (x == nodes[xParent][LEFT]) {
nodes[xParent][LEFT] = y;
        } else {
nodes[xParent][RIGHT] = y;
        }
nodes[y][LEFT] = x;
nodes[x][PARENT] = y;
t.nodes = nodes;
        return t;
    }

    static RBTree rotate_right(RBTree t, int x) {
        int[][] nodes_1 = ((int[][])(t.nodes));
        int y_1 = nodes_1[x][LEFT];
        int yRight = nodes_1[y_1][RIGHT];
nodes_1[x][LEFT] = yRight;
        if (yRight != NEG_ONE) {
nodes_1[yRight][PARENT] = x;
        }
        int xParent_1 = nodes_1[x][PARENT];
nodes_1[y_1][PARENT] = xParent_1;
        if (xParent_1 == NEG_ONE) {
t.root = y_1;
        } else         if (x == nodes_1[xParent_1][RIGHT]) {
nodes_1[xParent_1][RIGHT] = y_1;
        } else {
nodes_1[xParent_1][LEFT] = y_1;
        }
nodes_1[y_1][RIGHT] = x;
nodes_1[x][PARENT] = y_1;
t.nodes = nodes_1;
        return t;
    }

    static RBTree insert_fix(RBTree t, int z) {
        int[][] nodes_2 = ((int[][])(t.nodes));
        while (z != t.root && nodes_2[nodes_2[z][PARENT]][COLOR] == 1) {
            if (nodes_2[z][PARENT] == nodes_2[nodes_2[nodes_2[z][PARENT]][PARENT]][LEFT]) {
                int y_2 = nodes_2[nodes_2[nodes_2[z][PARENT]][PARENT]][RIGHT];
                if (y_2 != NEG_ONE && nodes_2[y_2][COLOR] == 1) {
nodes_2[nodes_2[z][PARENT]][COLOR] = 0;
nodes_2[y_2][COLOR] = 0;
                    int gp = nodes_2[nodes_2[z][PARENT]][PARENT];
nodes_2[gp][COLOR] = 1;
                    z = gp;
                } else {
                    if (z == nodes_2[nodes_2[z][PARENT]][RIGHT]) {
                        z = nodes_2[z][PARENT];
t.nodes = nodes_2;
                        t = rotate_left(t, z);
                        nodes_2 = ((int[][])(t.nodes));
                    }
nodes_2[nodes_2[z][PARENT]][COLOR] = 0;
                    int gp_1 = nodes_2[nodes_2[z][PARENT]][PARENT];
nodes_2[gp_1][COLOR] = 1;
t.nodes = nodes_2;
                    t = rotate_right(t, gp_1);
                    nodes_2 = ((int[][])(t.nodes));
                }
            } else {
                int y_3 = nodes_2[nodes_2[nodes_2[z][PARENT]][PARENT]][LEFT];
                if (y_3 != NEG_ONE && nodes_2[y_3][COLOR] == 1) {
nodes_2[nodes_2[z][PARENT]][COLOR] = 0;
nodes_2[y_3][COLOR] = 0;
                    int gp_2 = nodes_2[nodes_2[z][PARENT]][PARENT];
nodes_2[gp_2][COLOR] = 1;
                    z = gp_2;
                } else {
                    if (z == nodes_2[nodes_2[z][PARENT]][LEFT]) {
                        z = nodes_2[z][PARENT];
t.nodes = nodes_2;
                        t = rotate_right(t, z);
                        nodes_2 = ((int[][])(t.nodes));
                    }
nodes_2[nodes_2[z][PARENT]][COLOR] = 0;
                    int gp_3 = nodes_2[nodes_2[z][PARENT]][PARENT];
nodes_2[gp_3][COLOR] = 1;
t.nodes = nodes_2;
                    t = rotate_left(t, gp_3);
                    nodes_2 = ((int[][])(t.nodes));
                }
            }
        }
        nodes_2 = ((int[][])(t.nodes));
nodes_2[t.root][COLOR] = 0;
t.nodes = nodes_2;
        return t;
    }

    static RBTree tree_insert(RBTree t, int v) {
        int[][] nodes_3 = ((int[][])(t.nodes));
        int[] node = ((int[])(new int[]{v, 1, -1, -1, -1}));
        nodes_3 = ((int[][])(appendObj(nodes_3, node)));
        int idx = nodes_3.length - 1;
        int y_4 = NEG_ONE;
        int x = t.root;
        while (x != NEG_ONE) {
            y_4 = x;
            if (v < nodes_3[x][LABEL]) {
                x = nodes_3[x][LEFT];
            } else {
                x = nodes_3[x][RIGHT];
            }
        }
nodes_3[idx][PARENT] = y_4;
        if (y_4 == NEG_ONE) {
t.root = idx;
        } else         if (v < nodes_3[y_4][LABEL]) {
nodes_3[y_4][LEFT] = idx;
        } else {
nodes_3[y_4][RIGHT] = idx;
        }
t.nodes = nodes_3;
        t = insert_fix(t, idx);
        return t;
    }

    static int[] inorder(RBTree t, int x, int[] acc) {
        if (x == NEG_ONE) {
            return acc;
        }
        acc = ((int[])(inorder(t, t.nodes[x][LEFT], ((int[])(acc)))));
        acc = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(acc), java.util.stream.IntStream.of(t.nodes[x][LABEL])).toArray()));
        acc = ((int[])(inorder(t, t.nodes[x][RIGHT], ((int[])(acc)))));
        return acc;
    }

    static void main() {
        RBTree t = make_tree();
        int[] values = ((int[])(new int[]{10, 20, 30, 15, 25, 5, 1}));
        int i = 0;
        while (i < values.length) {
            t = tree_insert(t, values[i]);
            i = i + 1;
        }
        int[] res = ((int[])(new int[]{}));
        res = ((int[])(inorder(t, t.root, ((int[])(res)))));
        System.out.println(_p(res));
    }
    public static void main(String[] args) {
        LABEL = 0;
        COLOR = 1;
        PARENT = 2;
        LEFT = 3;
        RIGHT = 4;
        NEG_ONE = -1;
        main();
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
