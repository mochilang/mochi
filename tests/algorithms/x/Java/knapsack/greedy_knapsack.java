public class Main {

    static double calc_profit(int[] profit, int[] weight, int max_weight) {
        if (profit.length != weight.length) {
            throw new RuntimeException(String.valueOf("The length of profit and weight must be same."));
        }
        if (max_weight <= 0) {
            throw new RuntimeException(String.valueOf("max_weight must greater than zero."));
        }
        int i = 0;
        while (i < profit.length) {
            if (profit[i] < 0) {
                throw new RuntimeException(String.valueOf("Profit can not be negative."));
            }
            if (weight[i] < 0) {
                throw new RuntimeException(String.valueOf("Weight can not be negative."));
            }
            i = i + 1;
        }
        int n = profit.length;
        boolean[] used = ((boolean[])(new boolean[]{}));
        int j = 0;
        while (j < n) {
            used = ((boolean[])(appendBool(used, false)));
            j = j + 1;
        }
        int limit = 0;
        double gain = 0.0;
        int count = 0;
        while (limit < max_weight && count < n) {
            double maxRatio = -1.0;
            int maxIndex = -1;
            int k = 0;
            while (k < n) {
                if (!(Boolean)used[k]) {
                    double ratio = (((double)(profit[k]))) / (((double)(weight[k])));
                    if (ratio > maxRatio) {
                        maxRatio = ratio;
                        maxIndex = k;
                    }
                }
                k = k + 1;
            }
            if (maxIndex < 0) {
                break;
            }
used[maxIndex] = true;
            if (max_weight - limit >= weight[maxIndex]) {
                limit = limit + weight[maxIndex];
                gain = gain + (((double)(profit[maxIndex])));
            } else {
                gain = gain + (((Number)((max_weight - limit))).doubleValue() / (((double)(weight[maxIndex])))) * (((double)(profit[maxIndex])));
                break;
            }
            count = count + 1;
        }
        return gain;
    }

    static void main() {
        System.out.println(calc_profit(((int[])(new int[]{1, 2, 3})), ((int[])(new int[]{3, 4, 5})), 15));
        System.out.println(calc_profit(((int[])(new int[]{10, 9, 8})), ((int[])(new int[]{3, 4, 5})), 25));
        System.out.println(calc_profit(((int[])(new int[]{10, 9, 8})), ((int[])(new int[]{3, 4, 5})), 5));
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
