public class Main {

    static int knapsack(int[] weights, int[] values, int number_of_items, int max_weight, int index) {
        if (index == number_of_items) {
            return 0;
        }
        int ans1 = knapsack(((int[])(weights)), ((int[])(values)), number_of_items, max_weight, index + 1);
        int ans2 = 0;
        if (weights[index] <= max_weight) {
            ans2 = values[index] + knapsack(((int[])(weights)), ((int[])(values)), number_of_items, max_weight - weights[index], index + 1);
        }
        if (ans1 > ans2) {
            return ans1;
        }
        return ans2;
    }

    static void main() {
        int[] w1 = ((int[])(new int[]{1, 2, 4, 5}));
        int[] v1 = ((int[])(new int[]{5, 4, 8, 6}));
        int r1 = knapsack(((int[])(w1)), ((int[])(v1)), 4, 5, 0);
        System.out.println(_p(r1));
        int[] w2 = ((int[])(new int[]{3, 4, 5}));
        int[] v2 = ((int[])(new int[]{10, 9, 8}));
        int r2 = knapsack(((int[])(w2)), ((int[])(v2)), 3, 25, 0);
        System.out.println(_p(r2));
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
