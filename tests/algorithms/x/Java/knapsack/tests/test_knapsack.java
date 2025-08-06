public class Main {

    static int knapsack(int capacity, int[] weights, int[] values, int counter) {
        if (counter == 0 || capacity == 0) {
            return 0;
        }
        if (weights[counter - 1] > capacity) {
            return knapsack(capacity, ((int[])(weights)), ((int[])(values)), counter - 1);
        }
        int left_capacity = capacity - weights[counter - 1];
        int include_val = values[counter - 1] + knapsack(left_capacity, ((int[])(weights)), ((int[])(values)), counter - 1);
        int exclude_val = knapsack(capacity, ((int[])(weights)), ((int[])(values)), counter - 1);
        if (include_val > exclude_val) {
            return include_val;
        }
        return exclude_val;
    }

    static boolean test_base_case() {
        int cap = 0;
        int[] val = ((int[])(new int[]{0}));
        int[] w = ((int[])(new int[]{0}));
        int c = val.length;
        if (knapsack(cap, ((int[])(w)), ((int[])(val)), c) != 0) {
            return false;
        }
        int[] val2 = ((int[])(new int[]{60}));
        int[] w2 = ((int[])(new int[]{10}));
        int c2 = val2.length;
        return knapsack(cap, ((int[])(w2)), ((int[])(val2)), c2) == 0;
    }

    static boolean test_easy_case() {
        int cap_1 = 3;
        int[] val_1 = ((int[])(new int[]{1, 2, 3}));
        int[] w_1 = ((int[])(new int[]{3, 2, 1}));
        int c_1 = val_1.length;
        return knapsack(cap_1, ((int[])(w_1)), ((int[])(val_1)), c_1) == 5;
    }

    static boolean test_knapsack() {
        int cap_2 = 50;
        int[] val_2 = ((int[])(new int[]{60, 100, 120}));
        int[] w_2 = ((int[])(new int[]{10, 20, 30}));
        int c_2 = val_2.length;
        return knapsack(cap_2, ((int[])(w_2)), ((int[])(val_2)), c_2) == 220;
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(test_base_case());
            System.out.println(test_easy_case());
            System.out.println(test_knapsack());
            System.out.println(true ? "True" : "False");
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
