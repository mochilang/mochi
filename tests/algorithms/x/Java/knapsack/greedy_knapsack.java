public class Main {

    static double calc_profit(long[] profit, long[] weight, long max_weight) {
        if (profit.length != weight.length) {
            throw new RuntimeException(String.valueOf("The length of profit and weight must be same."));
        }
        if (max_weight <= 0) {
            throw new RuntimeException(String.valueOf("max_weight must greater than zero."));
        }
        long i_1 = 0L;
        while (i_1 < profit.length) {
            if (profit[(int)((long)(i_1))] < 0) {
                throw new RuntimeException(String.valueOf("Profit can not be negative."));
            }
            if (weight[(int)((long)(i_1))] < 0) {
                throw new RuntimeException(String.valueOf("Weight can not be negative."));
            }
            i_1 = i_1 + 1;
        }
        long n_1 = profit.length;
        boolean[] used_1 = ((boolean[])(new boolean[]{}));
        long j_1 = 0L;
        while (j_1 < n_1) {
            used_1 = ((boolean[])(appendBool(used_1, false)));
            j_1 = j_1 + 1;
        }
        long limit_1 = 0L;
        double gain_1 = 0.0;
        long count_1 = 0L;
        while (limit_1 < max_weight && count_1 < n_1) {
            double maxRatio_1 = -1.0;
            long maxIndex_1 = -1;
            long k_1 = 0L;
            while (k_1 < n_1) {
                if (!(Boolean)used_1[(int)((long)(k_1))]) {
                    double ratio_1 = (((double)(profit[(int)((long)(k_1))]))) / (((double)(weight[(int)((long)(k_1))])));
                    if (ratio_1 > maxRatio_1) {
                        maxRatio_1 = ratio_1;
                        maxIndex_1 = k_1;
                    }
                }
                k_1 = k_1 + 1;
            }
            if (maxIndex_1 < 0) {
                break;
            }
used_1[(int)((long)(maxIndex_1))] = true;
            if (max_weight - limit_1 >= weight[(int)((long)(maxIndex_1))]) {
                limit_1 = limit_1 + weight[(int)((long)(maxIndex_1))];
                gain_1 = gain_1 + (((double)(profit[(int)((long)(maxIndex_1))])));
            } else {
                gain_1 = gain_1 + (((Number)((max_weight - limit_1))).doubleValue() / (((double)(weight[(int)((long)(maxIndex_1))])))) * (((double)(profit[(int)((long)(maxIndex_1))])));
                break;
            }
            count_1 = count_1 + 1;
        }
        return gain_1;
    }

    static void main() {
        System.out.println(calc_profit(((long[])(new long[]{1, 2, 3})), ((long[])(new long[]{3, 4, 5})), 15L));
        System.out.println(calc_profit(((long[])(new long[]{10, 9, 8})), ((long[])(new long[]{3, 4, 5})), 25L));
        System.out.println(calc_profit(((long[])(new long[]{10, 9, 8})), ((long[])(new long[]{3, 4, 5})), 5L));
    }
    public static void main(String[] args) {
        main();
    }

    static boolean[] appendBool(boolean[] arr, boolean v) {
        boolean[] out = java.util.Arrays.copyOf(arr, arr.length + 1);
        out[arr.length] = v;
        return out;
    }
}
