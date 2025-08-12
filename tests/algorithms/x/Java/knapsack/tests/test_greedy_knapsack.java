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


    static CalcResult calc_profit(long[] profit, long[] weight, long max_weight) {
        if (profit.length != weight.length) {
            return new CalcResult(false, 0.0, "The length of profit and weight must be same.");
        }
        if (max_weight <= 0) {
            return new CalcResult(false, 0.0, "max_weight must greater than zero.");
        }
        long i_1 = 0L;
        while (i_1 < profit.length) {
            if (profit[(int)((long)(i_1))] < 0) {
                return new CalcResult(false, 0.0, "Profit can not be negative.");
            }
            if (weight[(int)((long)(i_1))] < 0) {
                return new CalcResult(false, 0.0, "Weight can not be negative.");
            }
            i_1 = i_1 + 1;
        }
        boolean[] used_1 = ((boolean[])(new boolean[]{}));
        long j_1 = 0L;
        while (j_1 < profit.length) {
            used_1 = ((boolean[])(appendBool(used_1, false)));
            j_1 = j_1 + 1;
        }
        long limit_1 = 0L;
        double gain_1 = 0.0;
        while (limit_1 < max_weight) {
            double max_ratio_1 = -1.0;
            long idx_1 = 0 - 1;
            long k_1 = 0L;
            while (k_1 < profit.length) {
                if (!(Boolean)used_1[(int)((long)(k_1))]) {
                    double ratio_1 = (((double)(profit[(int)((long)(k_1))]))) / (((double)(weight[(int)((long)(k_1))])));
                    if (ratio_1 > max_ratio_1) {
                        max_ratio_1 = ratio_1;
                        idx_1 = k_1;
                    }
                }
                k_1 = k_1 + 1;
            }
            if (idx_1 == 0 - 1) {
                break;
            }
used_1[(int)((long)(idx_1))] = true;
            if (max_weight - limit_1 >= weight[(int)((long)(idx_1))]) {
                limit_1 = limit_1 + weight[(int)((long)(idx_1))];
                gain_1 = gain_1 + (((double)(profit[(int)((long)(idx_1))])));
            } else {
                gain_1 = gain_1 + ((((Number)((max_weight - limit_1))).doubleValue()) / (((double)(weight[(int)((long)(idx_1))])))) * (((double)(profit[(int)((long)(idx_1))])));
                break;
            }
        }
        return new CalcResult(true, gain_1, "");
    }

    static boolean test_sorted() {
        long[] profit = ((long[])(new long[]{10, 20, 30, 40, 50, 60}));
        long[] weight_1 = ((long[])(new long[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_1 = calc_profit(((long[])(profit)), ((long[])(weight_1)), 100L);
        return res_1.ok && res_1.value == 210.0;
    }

    static boolean test_negative_max_weight() {
        long[] profit_1 = ((long[])(new long[]{10, 20, 30, 40, 50, 60}));
        long[] weight_3 = ((long[])(new long[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_3 = calc_profit(((long[])(profit_1)), ((long[])(weight_3)), -15);
        return !res_3.ok && (res_3.error.equals("max_weight must greater than zero."));
    }

    static boolean test_negative_profit_value() {
        long[] profit_2 = ((long[])(new long[]{10, -20, 30, 40, 50, 60}));
        long[] weight_5 = ((long[])(new long[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_5 = calc_profit(((long[])(profit_2)), ((long[])(weight_5)), 15L);
        return !res_5.ok && (res_5.error.equals("Profit can not be negative."));
    }

    static boolean test_negative_weight_value() {
        long[] profit_3 = ((long[])(new long[]{10, 20, 30, 40, 50, 60}));
        long[] weight_7 = ((long[])(new long[]{2, -4, 6, -8, 10, 12}));
        CalcResult res_7 = calc_profit(((long[])(profit_3)), ((long[])(weight_7)), 15L);
        return !res_7.ok && (res_7.error.equals("Weight can not be negative."));
    }

    static boolean test_null_max_weight() {
        long[] profit_4 = ((long[])(new long[]{10, 20, 30, 40, 50, 60}));
        long[] weight_9 = ((long[])(new long[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_9 = calc_profit(((long[])(profit_4)), ((long[])(weight_9)), 0L);
        return !res_9.ok && (res_9.error.equals("max_weight must greater than zero."));
    }

    static boolean test_unequal_list_length() {
        long[] profit_5 = ((long[])(new long[]{10, 20, 30, 40, 50}));
        long[] weight_11 = ((long[])(new long[]{2, 4, 6, 8, 10, 12}));
        CalcResult res_11 = calc_profit(((long[])(profit_5)), ((long[])(weight_11)), 100L);
        return !res_11.ok && (res_11.error.equals("The length of profit and weight must be same."));
    }
    public static void main(String[] args) {
        System.out.println(test_sorted());
        System.out.println(test_negative_max_weight());
        System.out.println(test_negative_profit_value());
        System.out.println(test_negative_weight_value());
        System.out.println(test_null_max_weight());
        System.out.println(test_unequal_list_length());
        System.out.println(true ? "True" : "False");
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
