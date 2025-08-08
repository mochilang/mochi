public class Main {

    static String list_to_string(int[] xs) {
        if (xs.length == 0) {
            return "";
        }
        String s = _p(_geti(xs, 0));
        int i = 1;
        while (i < xs.length) {
            s = s + "->" + _p(_geti(xs, i));
            i = i + 1;
        }
        return s;
    }

    static int[] insert_node(int[] xs, int data) {
        return java.util.stream.IntStream.concat(java.util.Arrays.stream(xs), java.util.stream.IntStream.of(data)).toArray();
    }

    static int[] rotate_to_the_right(int[] xs, int places) {
        if (xs.length == 0) {
            throw new RuntimeException(String.valueOf("The linked list is empty."));
        }
        int n = xs.length;
        int k = Math.floorMod(places, n);
        if (k == 0) {
            return xs;
        }
        int split = n - k;
        int[] res = ((int[])(new int[]{}));
        int i_1 = split;
        while (i_1 < n) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[i_1])).toArray()));
            i_1 = i_1 + 1;
        }
        int j = 0;
        while (j < split) {
            res = ((int[])(java.util.stream.IntStream.concat(java.util.Arrays.stream(res), java.util.stream.IntStream.of(xs[j])).toArray()));
            j = j + 1;
        }
        return res;
    }

    static void main() {
        int[] head = ((int[])(new int[]{}));
        head = ((int[])(insert_node(((int[])(head)), 5)));
        head = ((int[])(insert_node(((int[])(head)), 1)));
        head = ((int[])(insert_node(((int[])(head)), 2)));
        head = ((int[])(insert_node(((int[])(head)), 4)));
        head = ((int[])(insert_node(((int[])(head)), 3)));
        System.out.println("Original list: " + String.valueOf(list_to_string(((int[])(head)))));
        int places = 3;
        int[] new_head = ((int[])(rotate_to_the_right(((int[])(head)), places)));
        System.out.println("After " + _p(places) + " iterations: " + String.valueOf(list_to_string(((int[])(new_head)))));
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

    static Integer _geti(int[] a, int i) {
        return (i >= 0 && i < a.length) ? a[i] : null;
    }
}
