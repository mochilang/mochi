public class Main {

    static int knapsack(int capacity, int[] weights, int[] values, int counter) {
        if (counter == 0 || capacity == 0) {
            return 0;
        }
        if (weights[counter - 1] > capacity) {
            return knapsack(capacity, ((int[])(weights)), ((int[])(values)), counter - 1);
        } else {
            int left_capacity = capacity - weights[counter - 1];
            int new_value_included = values[counter - 1] + knapsack(left_capacity, ((int[])(weights)), ((int[])(values)), counter - 1);
            int without_new_value = knapsack(capacity, ((int[])(weights)), ((int[])(values)), counter - 1);
            if (new_value_included > without_new_value) {
                return new_value_included;
            } else {
                return without_new_value;
            }
        }
    }

    static void main() {
        int[] weights = ((int[])(new int[]{10, 20, 30}));
        int[] values = ((int[])(new int[]{60, 100, 120}));
        int cap = 50;
        int count = values.length;
        int result = knapsack(cap, ((int[])(weights)), ((int[])(values)), count);
        System.out.println(_p(result));
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
