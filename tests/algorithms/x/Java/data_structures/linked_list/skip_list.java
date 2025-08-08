public class Main {
    static int NIL;
    static int MAX_LEVEL;
    static double P;
    static int seed = 0;
    static int[] node_keys = new int[0];
    static int[] node_vals = new int[0];
    static int[][] node_forwards = new int[0][];
    static int level = 0;

    static double random() {
        seed = Math.floorMod((seed * 13 + 7), 100);
        return (((Number)(seed)).doubleValue()) / 100.0;
    }

    static int random_level() {
        int lvl = 1;
        while (random() < P && lvl < MAX_LEVEL) {
            lvl = lvl + 1;
        }
        return lvl;
    }

    static int[] empty_forward() {
        int[] f = ((int[])(new int[]{}));
        int i = 0;
        while (i < MAX_LEVEL) {
            f = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(f), java.util.stream.IntStream.of(NIL)).toArray()));
            i = i + 1;
        }
        return f;
    }

    static void init() {
        node_keys = ((int[])(new int[]{-1}));
        node_vals = ((int[])(new int[]{0}));
        node_forwards = ((int[][])(new int[][]{empty_forward()}));
        level = 1;
    }

    static void insert(int key, int value) {
        int[] update = ((int[])(new int[]{}));
        int i_1 = 0;
        while (i_1 < MAX_LEVEL) {
            update = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(update), java.util.stream.IntStream.of(0)).toArray()));
            i_1 = i_1 + 1;
        }
        int x = 0;
        i_1 = level - 1;
        while (i_1 >= 0) {
            while (node_forwards[x][i_1] != NIL && node_keys[node_forwards[x][i_1]] < key) {
                x = node_forwards[x][i_1];
            }
update[i_1] = x;
            i_1 = i_1 - 1;
        }
        x = node_forwards[x][0];
        if (x != NIL && node_keys[x] == key) {
node_vals[x] = value;
            return;
        }
        int lvl_1 = random_level();
        if (lvl_1 > level) {
            int j = level;
            while (j < lvl_1) {
update[j] = 0;
                j = j + 1;
            }
            level = lvl_1;
        }
        node_keys = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(node_keys), java.util.stream.IntStream.of(key)).toArray()));
        node_vals = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(node_vals), java.util.stream.IntStream.of(value)).toArray()));
        int[] forwards = ((int[])(empty_forward()));
        int idx = node_keys.length - 1;
        i_1 = 0;
        while (i_1 < lvl_1) {
forwards[i_1] = node_forwards[update[i_1]][i_1];
node_forwards[update[i_1]][i_1] = idx;
            i_1 = i_1 + 1;
        }
        node_forwards = ((int[][])(appendObj((int[][])node_forwards, forwards)));
    }

    static int find(int key) {
        int x_1 = 0;
        int i_2 = level - 1;
        while (i_2 >= 0) {
            while (node_forwards[x_1][i_2] != NIL && node_keys[node_forwards[x_1][i_2]] < key) {
                x_1 = node_forwards[x_1][i_2];
            }
            i_2 = i_2 - 1;
        }
        x_1 = node_forwards[x_1][0];
        if (x_1 != NIL && node_keys[x_1] == key) {
            return node_vals[x_1];
        }
        return -1;
    }

    static void delete(int key) {
        int[] update_1 = ((int[])(new int[]{}));
        int i_3 = 0;
        while (i_3 < MAX_LEVEL) {
            update_1 = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(update_1), java.util.stream.IntStream.of(0)).toArray()));
            i_3 = i_3 + 1;
        }
        int x_2 = 0;
        i_3 = level - 1;
        while (i_3 >= 0) {
            while (node_forwards[x_2][i_3] != NIL && node_keys[node_forwards[x_2][i_3]] < key) {
                x_2 = node_forwards[x_2][i_3];
            }
update_1[i_3] = x_2;
            i_3 = i_3 - 1;
        }
        x_2 = node_forwards[x_2][0];
        if (x_2 == NIL || node_keys[x_2] != key) {
            return;
        }
        i_3 = 0;
        while (i_3 < level) {
            if (node_forwards[update_1[i_3]][i_3] == x_2) {
node_forwards[update_1[i_3]][i_3] = node_forwards[x_2][i_3];
            }
            i_3 = i_3 + 1;
        }
        while (level > 1 && node_forwards[0][level - 1] == NIL) {
            level = level - 1;
        }
    }

    static String to_string() {
        String s = "";
        int x_3 = node_forwards[0][0];
        while (x_3 != NIL) {
            if (!(s.equals(""))) {
                s = s + " -> ";
            }
            s = s + _p(_geti(node_keys, x_3)) + ":" + _p(_geti(node_vals, x_3));
            x_3 = node_forwards[x_3][0];
        }
        return s;
    }

    static void main() {
        init();
        insert(2, 2);
        insert(4, 4);
        insert(6, 4);
        insert(4, 5);
        insert(8, 4);
        insert(9, 4);
        delete(4);
        System.out.println(to_string());
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NIL = 0 - 1;
            MAX_LEVEL = 6;
            P = 0.5;
            seed = 1;
            node_keys = ((int[])(new int[]{}));
            node_vals = ((int[])(new int[]{}));
            node_forwards = ((int[][])(new int[][]{}));
            level = 1;
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
