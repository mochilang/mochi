public class Main {
    static int NIL;
    static int seed = 0;
    static java.util.Map<String,Integer>[] nodes = new java.util.Map<String,Integer>[0];
    static int root = 0;

    static void set_seed(int s) {
        seed = s;
    }

    static int randint(int a, int b) {
        seed = ((int)(Math.floorMod(((long)((seed * 1103515245 + 12345))), 2147483648L)));
        return (Math.floorMod(seed, (b - a + 1))) + a;
    }

    static boolean rand_bool() {
        return randint(0, 1) == 1;
    }

    static void new_heap() {
        nodes = ((java.util.Map<String,Integer>[])((java.util.Map<String,Integer>[])new java.util.Map[]{}));
        root = NIL;
    }

    static int merge(int r1, int r2) {
        if (r1 == NIL) {
            return r2;
        }
        if (r2 == NIL) {
            return r1;
        }
        if ((int)(((int)(((java.util.Map)nodes[r1])).getOrDefault("value", 0))) > (int)(((int)(((java.util.Map)nodes[r2])).getOrDefault("value", 0)))) {
            int tmp = r1;
            r1 = r2;
            r2 = tmp;
        }
        if (((Boolean)(rand_bool()))) {
            int tmp_1 = (int)(((int)(((java.util.Map)nodes[r1])).getOrDefault("left", 0)));
nodes[r1]["left"] = ((int)(((java.util.Map)nodes[r1])).getOrDefault("right", 0));
nodes[r1]["right"] = tmp_1;
        }
nodes[r1]["left"] = merge((int)(((int)(((java.util.Map)nodes[r1])).getOrDefault("left", 0))), r2);
        return r1;
    }

    static void insert(int value) {
        java.util.Map<String,Integer> node = ((java.util.Map<String,Integer>)(new java.util.LinkedHashMap<String, Integer>(java.util.Map.ofEntries(java.util.Map.entry("value", value), java.util.Map.entry("left", NIL), java.util.Map.entry("right", NIL)))));
        nodes = ((java.util.Map<String,Integer>[])(appendObj((java.util.Map<String,Integer>[])nodes, node)));
        int idx = nodes.length - 1;
        root = merge(root, idx);
    }

    static int top() {
        if (root == NIL) {
            return 0;
        }
        return ((int)(((java.util.Map)nodes[root])).getOrDefault("value", 0));
    }

    static int pop() {
        int result = top();
        int l = (int)(((int)(((java.util.Map)nodes[root])).getOrDefault("left", 0)));
        int r = (int)(((int)(((java.util.Map)nodes[root])).getOrDefault("right", 0)));
        root = merge(l, r);
        return result;
    }

    static boolean is_empty() {
        return root == NIL;
    }

    static int[] to_sorted_list() {
        int[] res = ((int[])(new int[]{}));
        while (!(Boolean)is_empty()) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(pop())).toArray()));
        }
        return res;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            NIL = 0 - 1;
            seed = 1;
            nodes = ((java.util.Map<String,Integer>[])((java.util.Map<String,Integer>[])new java.util.Map[]{}));
            root = NIL;
            set_seed(1);
            new_heap();
            insert(2);
            insert(3);
            insert(1);
            insert(5);
            insert(1);
            insert(7);
            System.out.println(to_sorted_list());
            new_heap();
            insert(1);
            insert(-1);
            insert(0);
            System.out.println(to_sorted_list());
            new_heap();
            insert(3);
            insert(1);
            insert(3);
            insert(7);
            System.out.println(pop());
            System.out.println(pop());
            System.out.println(pop());
            System.out.println(pop());
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
}
