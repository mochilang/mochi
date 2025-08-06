public class Main {
    static class CalcResult {
        boolean ok;
        double value;
        String error;
        CalcResult(boolean ok, double value, String error) {
            this.ok = ok;
            this.value = value;
            this.error = error;
        }
        CalcResult() {}
        @Override public String toString() {
            return String.format("{'ok': %s, 'value': %s, 'error': '%s'}", String.valueOf(ok), String.valueOf(value), String.valueOf(error));
        }
    }


    static CalcResult calc_profit(int[] profit, int[] weight, int max_weight) {
        if (profit.length != weight.length) {
            return new CalcResult(false, 0.0, "The length of profit and weight must be same.");
        }
        if (max_weight <= 0) {
            return new CalcResult(false, 0.0, "max_weight must greater than zero.");
        }
        int i = 0;
        while (i < profit.length) {
            if (profit[i] < 0) {
                return new CalcResult(false, 0.0, "Profit can not be negative.");
            }
            if (weight[i] < 0) {
                return new CalcResult(false, 0.0, "Weight can not be negative.");
            }
            i = i + 1;
        }
        boolean[] used = ((boolean[])(new boolean[]{}));
        int j = 0;
        while (j < profit.length) {
            used = ((boolean[])(appendBool(used, false)));
            j = j + 1;
        }
        int limit = 0;
        double gain = 0.0;
        while (limit < max_weight) {
            double max_ratio = -1.0;
            int idx = 0 - 1;
            int k = 0;
            while (k < profit.length) {
                if (!(Boolean)used[k]) {
                    double ratio = (((double)(profit[k]))) / (((double)(weight[k])));
                    if (ratio > max_ratio) {
                        max_ratio = ratio;
                        idx = k;
                    }
                }
                k = k + 1;
            }
            if (idx == 0 - 1) {
                break;
            }
used[idx] = true;
            if (max_weight - limit >= weight[idx]) {
                limit = limit + weight[idx];
                gain = gain + (((double)(profit[idx])));
            } else {
                gain = gain + ((((Number)((max_weight - limit))).doubleValue()) / (((double)(weight[idx])))) * (((double)(profit[idx])));
                break;
            }
        }
        return new CalcResult(true, gain, "");
    }

    static boolean test_sorted() {
        int[] profit = ((int[])(new int[]{10, 20, 30, 40, 50, 60}));
        int[] weight = ((int[])(new int[]{2, 4, 6, 8, 10, 12}));
        CalcResult res = calc_profit(((int[])(profit)), ((int[])(weight)), 100);
        return res.ok && res.value == 210.0;
    }

    static boolean test_negative_max_weight() {
        int[] profit_1 = ((int[])(new int[]{10, 20, 30, 40, 50, 60}));
        int[] weight_1 = ((int[])(new int[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_1 = calc_profit(((int[])(profit_1)), ((int[])(weight_1)), -15);
        return !res_1.ok && (res_1.error.equals("max_weight must greater than zero."));
    }

    static boolean test_negative_profit_value() {
        int[] profit_2 = ((int[])(new int[]{10, -20, 30, 40, 50, 60}));
        int[] weight_2 = ((int[])(new int[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_2 = calc_profit(((int[])(profit_2)), ((int[])(weight_2)), 15);
        return !res_2.ok && (res_2.error.equals("Profit can not be negative."));
    }

    static boolean test_negative_weight_value() {
        int[] profit_3 = ((int[])(new int[]{10, 20, 30, 40, 50, 60}));
        int[] weight_3 = ((int[])(new int[]{2, -4, 6, -8, 10, 12}));
        CalcResult res_3 = calc_profit(((int[])(profit_3)), ((int[])(weight_3)), 15);
        return !res_3.ok && (res_3.error.equals("Weight can not be negative."));
    }

    static boolean test_null_max_weight() {
        int[] profit_4 = ((int[])(new int[]{10, 20, 30, 40, 50, 60}));
        int[] weight_4 = ((int[])(new int[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_4 = calc_profit(((int[])(profit_4)), ((int[])(weight_4)), 0);
        return !res_4.ok && (res_4.error.equals("max_weight must greater than zero."));
    }

    static boolean test_unequal_list_length() {
        int[] profit_5 = ((int[])(new int[]{10, 20, 30, 40, 50}));
        int[] weight_5 = ((int[])(new int[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_5 = calc_profit(((int[])(profit_5)), ((int[])(weight_5)), 100);
        return !res_5.ok && (res_5.error.equals("The length of profit and weight must be same."));
    }
    public static void main(String[] args) {
        {
            long _benchStart = _now();
            long _benchMem = _mem();
            System.out.println(test_sorted());
            System.out.println(test_negative_max_weight());
            System.out.println(test_negative_profit_value());
            System.out.println(test_negative_weight_value());
            System.out.println(test_null_max_weight());
            System.out.println(test_unequal_list_length());
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

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
